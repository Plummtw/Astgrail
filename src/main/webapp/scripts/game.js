function output_diff_time(time){
  var i = time.split(",");
  var server = new Date(i[0], i[1] - 1, i[2], i[3], i[4], i[5]);
  var local  = new Date();
  document.write(Math.floor((local - server) / 1000));
}

function output_realtime(){
  var seconds, time, value;
  var left_seconds = Math.floor((end_date - new Date()) / 1000);

  if(left_seconds > 0){
    var left_time = new Date(0, 0, 0, 0, 0, left_seconds);
    seconds = Math.floor(12 * 60 * 60 * left_seconds / diff_seconds);
    time = new Date(0, 0, 0, 0, 0, seconds);
    value = sentence;
    if(time.getHours()   > 0){value += time.getHours()   + "時";}
    if(time.getMinutes() > 0){value += time.getMinutes() + "分";}
    value += "(實際時間 ";
    if(left_time.getHours()   > 0){value += left_time.getHours()   + "時";}
    if(left_time.getMinutes() > 0){value += left_time.getMinutes() + "分";}
    if(left_time.getSeconds() > 0){value += left_time.getSeconds() + "秒";}
    value += ")";
  }
  else{
    time = new Date(0, 0, 0, 0, 0, Math.abs(left_seconds));
    value = "超過時間 ";
    if(time.getHours()   > 0){value += time.getHours()   + "時";}
    if(time.getMinutes() > 0){value += time.getMinutes() + "分";}
    if(time.getSeconds() > 0){value += time.getSeconds() + "秒";}
  }
  document.realtime_form.output_realtime.value = value;
  tid = setTimeout("output_realtime()", 1000);
}