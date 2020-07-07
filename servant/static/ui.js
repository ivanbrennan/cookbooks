var servantApp = angular.module('servantApp', []);

servantApp.controller('servantController', ['$scope', '$http', '$interval',
  function($scope, $http, $interval) {
    $scope.q = '';
    $scope.count = 0;
    $scope.successes = 0;

    var f = function(data) {
      var x = data.x, y = data.y;
      if(x*x + y*y <= 1)
      {
        $scope.successes++;
      }
      $scope.count++;
      $scope.pi = 4*$scope.successes/$scope.count;
    };

    $scope.getPoint = function() {
      getPoint($http)
        .then(rsp => f(rsp.data))
        .catch(console.log);
    };

    $scope.getBooks = function() {
      getBooks($http, $scope.q)
        .then(rsp => {
          console.log(rsp.data);
          $scope.books = rsp.data.results;
        })
        .catch(console.log);
    };

    $scope.getBooks();

    $interval($scope.getPoint, 200);
  }]
);
