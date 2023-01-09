import 'dart:convert';

import 'package:cookie_jar/cookie_jar.dart';
import 'package:dio/dio.dart';
import 'package:dio_cookie_manager/dio_cookie_manager.dart';
import 'package:flutter/foundation.dart';
//import 'package:pretty_dio_logger/pretty_dio_logger.dart';

class SkyveRestClient {
  /// Set this value using --dart-define=SERVER_URL=http://xyz/etc/ when launching
  static const _baseUri = 'http://localhost:8080/skyve/';
  // static const _baseUri = 'http://10.0.2.2:8080/skyve/';

  static final instance = SkyveRestClient._internal();
  bool _loggedIn = false;

  final Dio _dio = Dio(BaseOptions(
    baseUrl: _baseUri,
  ));

  factory SkyveRestClient() {
    return instance;
  }

  SkyveRestClient._internal() {
    _dio.options.connectTimeout = 10000;

    // Manage cookies
    final cookieJar = CookieJar();
    _dio.interceptors.add(CookieManager(cookieJar));

    // Log requests
    // _dio.interceptors.add(PrettyDioLogger(
    //     requestHeader: true,
    //     requestBody: true,
    //     responseBody: true,
    //     responseHeader: true,
    //     error: true,
    //     compact: false,
    //     maxWidth: 90));
  }

  String getBaseUri() {
    return _baseUri;
  }

  bool get loggedIn {
    return _loggedIn;
  }

  Future<String> _fetch(String url) async {
    final Response<String> response = await _dio.get(url);
    if (response.data == null) {
      throw Exception('Response data was null');
    }

    return response.data!;
  }

  /// Attempt to login using the provided creds
  Future<bool> login(
      {required String username,
      required String password,
      String customer = 'default'}) async {
    bool result = await _doLogin(
        username: username, password: password, customer: customer);

    _loggedIn = result;
    return result;
  }

  Future<bool> _doLogin(
      {required String username,
      required String password,
      required String customer}) async {
    final params = {'password': password, 'username': '$customer/$username'};
    validateStatus(int? status) {
      return status != null && status >= 200 && status < 300;
    }

    /// In browser the 302 response will be followed without
    /// any chance of us touching it. On mobile the redirect
    /// will not be followed because we're doing a POST.
    try {
      Response<String> response = await _dio.post('loginAttempt',
          queryParameters: params,
          options: Options(validateStatus: validateStatus));
      // This'll throw an error when running anywhere except the browser

      // Big ol' hack here:
      // Search for this title string in the response,
      // this one is the /login?error page
      String responseContent = response.data ?? '';
      int idx = responseContent.indexOf('<title>Skyve: Sign In</title>');

      if (idx == -1) {
        return true;
      } else {
        return false;
      }
    } on DioError catch (err) {
      // On Android etc, we'll end up here

      // This URI thing is very dumb....
      const failureUriFragment = 'login?error';
      final locationHeader = err.response?.headers['location'];

      if (locationHeader == null || locationHeader.length != 1) {
        // Header wasn't present, or had more than one result
        return false;
      }

      // If the location header contains 'login?error' we haven't
      // logged in sucessfully
      return (!locationHeader[0].contains(failureUriFragment));
    }
  }

  Future<List<dynamic>> query(
      String moduleName, String queryName, int startRow, endRow) async {
    debugPrint('Fetch list $moduleName.$queryName');

    return await _fetch(
            'smartlist?_operationType=fetch&_dataSource=${moduleName}_$queryName&_startRow=$startRow&_endRow=$endRow')
        .then((jsonString) {
      var decoded = jsonDecode(jsonString);
      return decoded['response']['data'];
    });
  }

  Future<Map<String, dynamic>> metadata() async {
    debugPrint('Fetch metadata');

    return await _fetch('meta').then((jsonString) {
      return jsonDecode(jsonString);
    });
  }

  Future<Map<String, dynamic>> edit(
      String moduleName, String documentName, String? bizId) async {
    debugPrint('Edit bean $moduleName.$documentName#$bizId');

    String url =
        'smartedit?_operationType=fetch&_mod=$moduleName&_doc=$documentName&_ecnt=0&_ccnt=0';

    if (bizId != null) {
      url += '&bizId=$bizId';
    }

    return await _fetch(url).then((jsonString) {
      return jsonDecode(jsonString)['response']['data'][0];
    });
  }
}

class Result {
  late bool hasError;
  late String errorMessage;
  late int statusCode;

  Result(this.hasError, this.errorMessage, {this.statusCode = 0});
}
