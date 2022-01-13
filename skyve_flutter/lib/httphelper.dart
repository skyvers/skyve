import 'dart:convert';

import 'package:dio/dio.dart';

class SkyveFlutterRESTHelper {
  Dio? dio;
  Map<String, String> headers = new Map();

  SkyveFlutterRESTHelper() {
    headers["Authorization"] =
        'Basic ' + base64Encode('s3200/kevin:kevin'.codeUnits);
    var options = BaseOptions(
        baseUrl: 'http://192.168.2.199:8080/s3200EditorLibrarian/',
        connectTimeout: 5000,
        receiveTimeout: 3000,
        headers: headers);
    dio = Dio(options);
  }

  void getContact() async {
    try {
      var response = await dio?.get(
          '/rest/json/admin/Contact/52bc20df-e563-474d-a758-5ebc1b5992d0');
      print(response);
    } catch (e) {
      print(e);
    }
  }
}
