from flask import Flask, render_template, request
from transpiler import convert_cpp_to_python

app = Flask(__name__)

@app.route('/', methods=['GET', 'POST'])
def index():
    cpp_code = ''
    python_code = ''
    if request.method == 'POST':
        cpp_code = request.form['cpp_code']
        python_code = convert_cpp_to_python(cpp_code)
    return render_template('index.html', cpp_code=cpp_code, python_code=python_code)

if __name__ == '__main__':
    app.run(debug=True)