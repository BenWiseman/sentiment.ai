var coll = document.getElementsByClassName("collapsible");
var i;

for (i = 0; i < coll.length; i++) {

  coll[i].addEventListener("click", function() {

    this.classList.toggle("active");
    var content = this.nextElementSibling;
    if (content.style.display === "block") {
      content.style.display = "none";
    } else {
      content.style.display = "block";
    }


  });


}

document.getElementsByClassName("collapsible active").textContent = 'Show Filter';


function change(){

    for (i = 0; i < coll.length; i++) {
        var this_value = coll[i].value;
        var this_thing = this_value.split(' ').slice(1).join(' ');
        if(this_value.match("^Close")) coll[i].value = "Open " + this_thing;
        else coll[i].value = "Close " + this_thing;

    }
}
