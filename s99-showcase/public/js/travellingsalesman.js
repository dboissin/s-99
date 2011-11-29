function poll(url, handler){
    $.ajax({ url: url, success: handler(data),
    dataType: "json", complete: poll, timeout: 30000 });
}