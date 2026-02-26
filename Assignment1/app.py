from flask import Flask, render_template
from university_system import UniversityAnalyzer

app = Flask(__name__)

@app.route("/")
def home():
    analyzer = UniversityAnalyzer()
    results = analyzer.integrate_all()
    return render_template("index.html", results=results)

if __name__ == "__main__":
    app.run(debug=True)