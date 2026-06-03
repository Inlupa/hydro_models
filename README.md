# 🏞️ Hydrological Modeling: HBV (LuMod, Python) & GR4J (airGR, R)

Репозиторий содержит примеры реализации концептуальных гидрологических моделей **HBV** и **GR4J** на языках Python и R соответственно. Эти модели использовались для исследования поведения речного стока на примере водосбора реки Вельва и Буктырма.

## 📁 Структура репозитория
├── data/ &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; # Метео- и гидрологические данные в формате CSV<br>
├── HBV_LuMod/ &nbsp;&nbsp;&nbsp; # Jupyter Notebook с реализацией HBV на Python<br>
├── GR4J_airGR/ &nbsp;&nbsp;&nbsp;&nbsp; # Скрипты и R-файлы для модели GR4J<br>
└── results/&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; # Графики, полученные после моделирования
---

## 🌀 HBV с использованием LuMod (Python)

Скрипты в `HBV_LuMod` демонстрируют цикл подготовки данных, моделирования и визуализации:

- Чтение CSV-файла с метеоданными;
- Преобразование и очистка данных;
- Настройка модели `HBV` с использованием библиотеки [LuMod](https://github.com/hydrogo/lumod);
- Задание диапазонов параметров;
- Калибровка модели методом Монте-Карло;
- Визуализация результатов;
- Расчёт метрик (NSE, R², RMSE и др.).

📌 Пример запуска (внутри Jupyter Notebook):

```python
model_HBV = HBV(area=830, lat=58.5)
model_HBV.fit(xobs=xobs, xsim=xsim, bounds=bounds, method="montecarlo")
