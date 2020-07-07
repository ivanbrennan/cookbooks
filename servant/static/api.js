app.service('', function($http) {
  return ({
var getPoint = function($http)
{
  return $http(
    { url: '/point'
    , method: 'GET'
    });
}
,
var getBooks = function($http, q)
{
  return $http(
    { url: '/books' + '?q=' + encodeURIComponent(q)
    , method: 'GET'
    });
}
});
});
