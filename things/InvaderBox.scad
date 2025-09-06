union () {
  translate ([0, 0, 0]) {
    cube ([262.0, 90, 2], center=true);
  }
  union () {
    translate ([-67.5, 0, 5]) {
      cube ([65.5, 90, 10], center=true);
    }
    translate ([0, 0, 15]) {
      cube ([65.5, 90, 30], center=true);
    }
    translate ([67.5, 0, 10]) {
      cube ([65.5, 90, 20], center=true);
    }
  }
  translate ([0, 0, 35]) {
    color ([0.5, 0.5, 0.5, 1]) {
      linear_extrude (height=5, center=true){
        polygon (points=[[14, 20], [9.24, 15.0], [7.0, 16.0], [4.62, 15.0], [0, 20], [3.5, 8.0], [0, 0], [7.0, 4.0], [14, 0], [10.5, 8.0]]);
      }
    }
  }
}
