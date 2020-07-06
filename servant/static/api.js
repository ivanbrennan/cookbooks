
var getPoint = function()
{
  return axios({ url: '/point'
    , method: 'get'
    });
}



var getBooks = function(q)
{
  return axios({ url: '/books' + '?q=' + encodeURIComponent(q)
    , method: 'get'
    });
}
