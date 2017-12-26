def init():
    return {
        'dataType': 'test',
        'computeWindowSec': 60,
        'computePeriodSec': 5
    }

def process(data, meta):
    result = {}

    result['avg']      = compute(transform(data))
    result['sensorId'] = data[0]['sensorId']
    result['start']    = meta['from']
    result['end']      = meta['to']

    return result

def transform(data):
    return [
        [x * 9.8 for x in accelerometer['data']] if accelerometer['dataUnit'] == 'g-force'
        else accelerometer['data']
        for accelerometer in data
    ]

def compute(data):
    count = len(data)
    x = 0
    y = 0
    z = 0

    for measure in data:
        x += measure[0]
        y += measure[1]
        z += measure[2]

    return {'x': round(float(x) / count, 2),
            'y': round(float(y) / count, 2),
            'z': round(float(z) / count, 2)}
