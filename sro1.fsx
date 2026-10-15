// ===== Готовый фрагмент: чтение CSV (скопируйте без изменений) =====
// readRows "data.csv" — читает файл рядом со скриптом,
// пропускает строку заголовков и пустые строки,
// возвращает список строк; каждая строка — массив ячеек.
let readRows (fileName: string) =
    let path = System.IO.Path.Combine(__SOURCE_DIRECTORY__, fileName)
    System.IO.File.ReadAllLines path
    |> Array.toList
    |> List.tail
    |> List.filter (fun line -> line.Trim() <> "")
    |> List.map (fun line ->
        line.Split ';' |> Array.map (fun cell -> cell.Trim()))

let toFloat (text: string) = float (text.Replace(',', '.'))

let toDate (text: string) =
    System.DateTime.ParseExact(text, [| "yyyy-MM-dd"; "dd.MM.yyyy" |], null,
                               System.Globalization.DateTimeStyles.None)

let showDate (date: System.DateTime) = date.ToString "dd.MM.yyyy"

// Описание записи Forecast для Варианта 2
type Forecast =
    { Date: System.DateTime
      Source: string
      PredictedMaxTemp: float
      ActualMaxTemp: float
      PredictedPrecip: int
      ActualPrecip: int }

// Превращает массив ячеек одной строки CSV в запись Forecast
let parseForecast (cells: string[]) =
    { Date = toDate cells[0]
      Source = cells[1]
      PredictedMaxTemp = toFloat cells[2]
      ActualMaxTemp = toFloat cells[3]
      PredictedPrecip = int cells[4]
      ActualPrecip = int cells[5] }

// Читаем данные из data.csv
let forecasts = readRows "data.csv" |> List.map parseForecast

// Ошибка прогноза со знаком (прогноз − факт)
let error f = f.PredictedMaxTemp - f.ActualMaxTemp

// Ошибка без знака (модуль ошибки)
let absError f = abs (error f)

// Средняя ошибка без знака по источникам (от точных к неточным)
let accuracyBySource fs =
    fs
    |> List.groupBy (fun f -> f.Source)
    |> List.map (fun (source, items) ->
        (source, items |> List.averageBy absError))
    |> List.sortBy (fun (_, avgErr) -> avgErr)

// Запись с самой большой ошибкой без знака
let biggestMiss fs =
    fs |> List.maxBy absError

// Процент записей, в которых прогноз осадков совпал с фактом
let rainHitPercent fs =
    let hits = fs |> List.filter (fun f -> f.PredictedPrecip = f.ActualPrecip) |> List.length
    float hits / float (List.length fs) * 100.0

// Процент совпадений осадков по источникам
let rainBySource fs =
    fs
    |> List.groupBy (fun f -> f.Source)
    |> List.map (fun (source, items) ->
        let hits = items |> List.filter (fun f -> f.PredictedPrecip = f.ActualPrecip) |> List.length
        let percent = float hits / float (List.length items) * 100.0
        (source, percent))

// Систематическая ошибка со знаком по источникам
let biasBySource fs =
    fs
    |> List.groupBy (fun f -> f.Source)
    |> List.map (fun (source, items) ->
        (source, items |> List.averageBy error))

// Словесное описание систематической ошибки
let biasWord (b: float) =
    if b > 0.0 then $"завышает в среднем на {abs b:F1} °C"
    elif b < 0.0 then $"занижает в среднем на {abs b:F1} °C"
    else "не ошибается"

// День с наибольшим разбросом температур между прогнозами источников
let mostDisputedDay fs =
    fs
    |> List.groupBy (fun f -> f.Date)
    |> List.map (fun (date, items) ->
        let maxP = items |> List.maxBy (fun f -> f.PredictedMaxTemp) |> _.PredictedMaxTemp
        let minP = items |> List.minBy (fun f -> f.PredictedMaxTemp) |> _.PredictedMaxTemp
        (date, maxP - minP))
    |> List.maxBy (fun (_, spread) -> spread)

// Своя метрика: Самый тёплый фактический день за весь период
let maxActualTemp fs =
    fs |> List.maxBy (fun f -> f.ActualMaxTemp)

// Функция печати отчёта
let printReport fs =
    printfn "Прогноз и реальность"
    printfn "Записей: %d" (List.length fs)
    
    printfn "Средняя ошибка прогноза максимума:"
    accuracyBySource fs
    |> List.iter (fun (source, err) -> printfn "  %s: %.1f °C" source err)
    
    let miss = biggestMiss fs
    printfn "Самый большой промах: %s, %s: прогноз %.1f, факт %.1f" 
        miss.Source (showDate miss.Date) miss.PredictedMaxTemp miss.ActualMaxTemp
        
    printfn "Угадали осадки:"
    rainBySource fs
    |> List.iter (fun (source, pct) -> printfn "  %s: %d%%" source (int pct))
    
    printfn "Систематическая ошибка:"
    biasBySource fs
    |> List.iter (fun (source, b) -> printfn "  %s: %s" source (biasWord b))
    
    let (d, spread) = mostDisputedDay fs
    printfn "Источники сильнее всего разошлись %s: разброс %.1f °C" (showDate d) spread
    
    let bestMax = maxActualTemp fs
    printfn "Своя метрика (Самый тёплый фактический день): %s (%.1f °C)" (showDate bestMax.Date) bestMax.ActualMaxTemp

do printReport forecasts