function setup() {
  createCanvas(600, 600);
  noLoop();
}

function draw() {
  // background colour for whole canvas
  background("#A0AAA6");
  strokeWeight(0);
  
  // base square
  fill("#DADDD8");
  rect(
    x = 0.1*width, y = 0.1*height,
    w = 0.8*width, y = 0.8*height);
   
  // large circle
  fill("#EEF0F2");
  circle(
    x = 0.575*width, y = 0.4*height, d = 0.5*width);
  
  // small circle
  fill("#ECEBE4");
  circle(
    x = 0.71*width, y = 0.6*height, d = 0.3*width);
  
   // triangle
  fill("#ECEBE4");
  triangle(
    x1 = 0.15*width, y1 = 0.15*height,
    x2 = 0.3*width, y2 = 0.25*height,
    x3 = 0.15*width, y3 = 0.55*height);
  
  // twisted quad
  fill("#ECEBE4");
  quad(
    x1 = 0.3*width, y1 = 0.3*height,
    x2 = 0.15*width, y2 = 0.6*height,
    x3 = 0.85*width, y3 = 0.85*height,
    x4 = 0.3*width, y4 = 0.85*height);
  
  // quad
  fill("#EEF0F2");
  quad(
    x1 = 0.15*width, y1 = 0.625*height,
    x2 = 0.15*width, y2 = 0.85*height,
    x3 = 0.275*width, y3 = 0.85*height,
    x4 = 0.275*width, y4 = 0.672*height);
  
}

function doubleClicked(){
  saveCanvas("20230113", "png");
}