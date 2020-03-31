#!/usr/bin/env python3
# -*- coding: utf-8 -*-


import pickle
from flask import Flask, request
from flasgger import Swagger
import numpy as np
import pandas as pd

with open('./rf.pkl', 'rb') as model_file:
    model = pickle.load(model_file)

app = Flask(__name__)
swagger = Swagger(app)

@app.route('/predict_file', methods=["POST"])

def predict_iris_file():  
    '''Example file endpoint returning a prediction of iris
    ---
    parameters:
      - name: input_file
        in: formData
        type: file
        required: true
    definitions:
        value:
            type: object
            properties:
                value_name:
                    type: string
                    items:
                        $ref: '#/definitions/Color'
        Color:
            type: string
    responses:
        200:
            description: OK
            schema:
                $ref: '#/definitions/value'
    '''
    input_data = pd.read_csv(request.files.get('input_file'), header = None)
    prediction = model.predict(input_data)
    return str(list(prediction))


if __name__ == '__main__':
    app.run(host='0.0.0.0', port=5000)
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    