const countries =  [
  {
    country: "Poland",
    startDate: [2016, 8, 3],
  },
  {
    country: "United States",
    startDate: [2017, 2, 24],
  },
  {
    country: "Australia",
    startDate: [2019, 1, 12],
  },
  {
    country: "China",
    startDate: [2017, 10, 15],
  },
  {
    country: "Germany",
    startDate: [2016, 11, 13],
  },
  {
    country: "United Kingdom",
    startDate: [2017, 1, 7],
  },
  {
    country: "Spain",
    startDate: [2019, 2, 27],
  },
  {
    country: "Sweden",
    startDate: [2018, 12, 1],
  },
  {
    country: "United Arab Emirates",
    startDate: [2019, 2, 12],
  },
  {
    country: "Switzerland",
    startDate: [2017, 1, 23],
  },
];

const getRandom = (bottomLimit, upperLimit) => {
  return (
    Math.floor(Math.random() * (upperLimit - bottomLimit + 1)) + bottomLimit
  );
};

const getDateString = (date) => date.toISOString().slice(0, 10);

const getData = (list) => {
  const todaysDate = new Date();
  const todaysDateString = getDateString(todaysDate);

  return list.map(element => {
    const {
      country,
      startDate: [year, month, day]
    } = element;

    const date = new Date(year, month - 1, day);
    let countryData = [];
    let isValid = true;

    // generate data for each consecutive date
    while (isValid) {
      const dateString = getDateString(date);
      date.setDate(date.getDate() + 1);
  
      countryData = [
        ...countryData,
        {
          country,
          date: dateString,
          income: getRandom(0, 3000),
          users: getRandom(0, 80),
          orders: getRandom(0, 50),
          complaints: getRandom(0, 5),
        },
      ];  
      if (dateString === todaysDateString) isValid = false;
    }
    return countryData;
  })
};
const dataset = getData(countries);

// create csv file
const csv = "data:text/csv;charset=utf-8,\n"
  + "country, date, income, users, orders, complaints\n"
  + dataset.map(country => country.map(entry =>
      Object.values(entry).join(",")).join("\n")).join("\n");

// download csv file
const encodedUri = encodeURI(csv);
const link = document.createElement("a");
link.setAttribute("href", encodedUri);
link.setAttribute("download", "dataset.csv");
document.body.appendChild(link);
link.click();