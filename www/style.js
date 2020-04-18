var inputs = document.getElementsByTagName("input");
//inputs.forEach(input => console.log(input));
for (var i in inputs) {
  console.log(i);
  console.log(inputs[i]);
  if (inputs[i].type == "radio") {
    console.log("Hello");
    inputs[i].type = "button";
    //inputs[i].getElementsByTagName("span")[0].html = "";
  }
}
