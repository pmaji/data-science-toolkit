# Script to run in Colab to setup dependencies and data

rm -rf requirements.txt*
rm -rf data
wget https://raw.githubusercontent.com/klemag/PyconUS_2019-model-interpretability-tutorial/master/requirements.txt
pip install -r requirements.txt
mkdir data
wget https://raw.githubusercontent.com/klemag/PyconUS_2019-model-interpretability-tutorial/master/data/bank.csv
mv bank.csv data
wget https://raw.githubusercontent.com/klemag/PyconUS_2019-model-interpretability-tutorial/master/data/toucan.jpg
mv toucan.jpg data
