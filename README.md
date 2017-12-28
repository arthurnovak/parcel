# parcel

Parcel service is a basic example of ETL platform.
It is responsible for aggregating data, transforming and computing it according
to custom business logic and delivering processed data to desired source.

Contents:
- [General Overview](#general-overview)
- [Content Management](#content-management)
  - [Content Location](#content-location)
  - [Content Workflow](#content-workflow)
  - [Update Content](#update-content)
  - [Basic structure](#basic-structure)
    - [Initialisation](#initialisation)
    - [Process](#process)
    - [Data](#data)
    - [Meta](#meta)
  - [Content Example](#content-example)
- [Stream Data](#stream-data)
- [Configuration](#configuration)
- [HTTP API Documentation](#http-api-documentation)
- [Dependencies](#dependencies)
- [Hot to Consume](#how-to-consume)
  - [Testing](#testing)
- [What is Next](#what-is-next)

## General Overview<a id="sec-1" name="sec-1"></a>

Parcel as an ETL service does 3-step work:

1. For aggregating data, parcel exposes its [API](#http-api-documentation) to collect measurements from
sensors for future processing;

2. Data transformation is done by applying customised content written in Python
to collected data;

3. Transformed results can be loaded in to different sources. Currently we support
Elastic Search only.

## Content Management<a id="sec-2" name="sec-2"></a>

Parcel service is driven by a set of python content files. This content describes
how collected data needs to be transformed and finally loaded in to desired source.

Python is chosen as specification language for several reasons:
 * Well known or easy to learn
 * Powerful enough and easy to use in simple transformations of `json` like structures
 * An existing `erlang` driver that is not in experimental state
 * Fast enough

### Content Location<a id="sec-2-1" name="sec-2-1"></a>
At the moment content is stored locally in `./priv/content` folder by default under
the following schema:
```
./priv/content/
     |-current.version <- contains current version of content to use (e.g. v1.0)
     |-v1.0.tar.gz     <- content archive
     |-v1.1.tar.gz
```

Custom content path can be changed by updating `content_path` parameter in
`.app.src` file (please see [Configuration](#configuration)).

Parcel polls `current.version` file for version change every
`content_poll_interval_sec` seconds (please see [Configuration](#configuration)).

When version is changed, the node will try to get content under new version, verify it and start using it internally.
If verification failed, log message is sent and service state is kept unchanged.

### Content Workflow<a id="sec-2-2" name="sec-2-2"></a>
Simplified content upgrade workflow:
 1. Content is uploaded, where it's verified and stored in `<version>.tar.gz` file;
 2. `current.version` is set to `<version>`;
 3. Eventually service will detect that `current.version` has changed and will pick
 up new content and update the state.

### Update Content<a id="sec-2-3" name="sec-2-3"></a>
Parcel service exposes [API](#http-api-documentation) for content management.

### Basic Structure<a id="sec-2-4" name="sec-2-4"></a>
Each Python content file describes a single data source that we wish to extract.
They can be placed in any folder structure so it is free to self-organise them
in a logical manner.

File names must be unique throughout the repository.

The following sections detail functions that must be defined in Python content to be
executed by parcel. Note that while these functions are required, content is **not** limited
to only these defined functions.

#### Initialisation<a id="sec-2-3-1" name="sec-2-3-1"></a>

The `init()` function always returns a dictionary containing a content specification:
-   **dataType:**
    A unique content dataType described by this file
-   **computeWindowSec:**
    An interval describes the size of compute window
-   **computePeriodSec:**
    A period of run computing

#### Process<a id="sec-2-3-2" name="sec-2-3-2"></a>
The `process(data, meta)` function defines a method that will be executed for each input data
record. The output of this function is a dictionary.

#### Data<a id="sec-2-3-3" name="sec-2-3-3"></a>
The input `data` object contains a Python dictionary corresponding to a `json` object.

#### Meta<a id="sec-2-3-4" name="sec-2-3-4"></a>
The metadata dictionary contains information about content execution. Metadata is built
on parcel backend side and send to `process` function.

Currently we support the following metadata keys:
- **from:**
  start date-time of computing window in `ISO8601` format
- **to:**
  end date-time of computing window in `ISO8601` format

### Content Example<a id="sec-2-4" name="sec-2-4"></a>
```python
def init():
    return {
        'dataType': 'accelerometer',
        'computeWindowSec': 60,
        'computePeriodSec': 5
    }

def process(data, meta):
    result = {}
    # some transformation logic here
    return result
```

## Stream Data<a id="sec-3" name="sec-3"></a>
When receiving a batch of records through API, parcel parses it and retrieves
`dataType` and `sensorId` properties. Based on combination of these values,
parcel tries to spawn a stream with `dataType-sensorId` name and put records to
that stream.

In case corresponding stream already exists, parcel just puts records into it.

Here we assume that records batch came from one source has similar `dataType` in it,
which means that one sensor cannot send data of different types.

In a situation when the content is updated and content file with corresponding `dataType`
is removed (so we don't want to process records of this `dataType` anymore),
parcel will terminate all streams with `dataType-...` names.

`ordered-set` ETS table with `createdTime` timestamp as a key is chosen as
a stream queueing system.

Parcel gets records out of stream queue once a `computePeriodSec` interval for period in
`computeWindowSec` back and pass it to Python process. Transformed results are loaded
to Elastic Search.

## Configuration<a id="sec-4" name="sec-4"></a>
Parcel configuration can be found in `.app.src` file.
It includes the following options:
```
        %% Period to check content folder for updated content and
        %% current.version file
        {content_poll_interval_sec, 30},

        %% Content folder path
        %% './priv' by default
        {content_path, "/some/path/"},

        %% Temp folder for wrapped python content
        %% '/tmp/parcel' by default
        {python_path_dir, "/tmp/parcel"},

        %% Elastic Search index
        {es_index, parcel}
```

## HTTP API Documentation<a id="sec-5" name="sec-5"></a>
To build documentation locally, you have to install [apidoc](http://apidocjs.com/).
Once you have it installed, you can build the documentation:

```sh
apidoc -v -i doc/ -o doc/api/ -f .erl
```

To view the documentation after it was built, open the file `./doc/api/index.html`.

## Dependencies<a id="sec-6" name="sec-6"></a>
Parcel service loads processed data to Elastic Search, which requires ES to be run
on same machine, where parcel is run.

`lager` is chosen as a standard logger. The service logs will be visible either in erlang
console or stored in `./log` folder.

## How to Consume<a id="sec-7" name="sec-7"></a>
1. Clone parcel service from github
```sh
git clone https://github.com/arthurnovak/parcel.git
cd parcel
```

2. Get all dependencies and compile the project
```
rebar g-d
rebar co
```

3. Run erlang console
```sh
erl -pa ebin -pa deps/*/ebin
```

4. Start the service
```
parcel:start().
```

5. Create content for your requirements or use default `accelerometer` content
already created and archived into `./priv/content/v1.0.tar.gz`

6. Put content and set content version via [API](#http-api-documentation)

7. Post measurements data json via [API](#http-api-documentation)

### Testing<a id="sec-7-1" name="sec-7-1"></a>

To run the unit tests alone, execute:

```sh
rebar eu
```

To run the common tests alone, execute:

```sh
rebar ct
```

## What is Next<a id="sec-8" name="sec-8"></a>
1. Storing content somewhere locally is a temporary solution. For future it
should live in specific `s3` bucket.
2. It makes sense to implement content validation API endpoint to post content
archive and receive schema validation result as a response.
3. We shouldn't be limited by Elastic Search as a single load source. To support
different sources, content init specification may include `loadSource` option.
