/* book search */
const $ = document.querySelector.bind(document);

function updateResults(data)
{
  console.log(data);
  $('#results').innerHTML = "";
  $('#query').innerHTML = "\"" + data.query + "\"";
  for(var i = 0; i < data.results.length; i++)
  {
    $('#results').appendChild(renderBook(data.results[i]));
  }
}

function renderBook(book)
{
  var li = document.createElement('LI');
  li.innerHTML = '<strong>' + book.title + '</strong>, <i>'
               + book.author + '</i> - ' + book.year;
  return li;
}

function searchBooks()
{
  var q = $('#q').value;
  getBooks(q)
    .then(rsp => updateResults(rsp.data))
    .catch(console.log)
}

searchBooks();
$('#q').addEventListener("keyup", function () {
  searchBooks();
});

/* approximating pi */
var count = 0;
var successes = 0;

function f(data)
{
  var x = data.x, y = data.y;
  if(x*x + y*y <= 1)
  {
    successes++;
  }

  count++;

  update('#count', count);
  update('#successes', successes);
  update('#pi', 4*successes/count);
}

function update(id, val)
{
  $(id).innerHTML = val;
}

function refresh()
{
  getPoint()
    .then(rsp => f(rsp.data))
    .catch(console.log);
}

window.setInterval(refresh, 200);
