import 'dart:convert';
import 'dart:io';

import 'package:dio/dio.dart';
import 'package:flutter/foundation.dart';
import 'package:http/http.dart' as http;
import 'package:flutter_secure_storage/flutter_secure_storage.dart';

class SkyveRestClient {
  static const int ok = 200;

  /// Set this value using --dart-define=SERVER_URL=http://xyz/etc/ when launching
  static const _baseUri =
      'http://localhost:8080/skyve/'; //String.fromEnvironment('SERVER_URL');

  /// Secure key-value store from flutter_secure_storage
  static const _storage = FlutterSecureStorage();
  static const _authStorageKey = 'Authorization';

  final Dio _dio = Dio();

  SkyveRestClient() {
    // dio options, interceptors, etc
    _dio.options.connectTimeout = 10000;
  }

  String getBaseUri() {
    // if (kIsWeb) {
    //   return '../api/';
    // }

    // return _baseUri + "api/";
    return _baseUri;
  }

  Future<http.Response> _fetch(String url, String body) async {
    final String uri = getBaseUri() + url;
    debugPrint(uri);
    final Map<String, String> headers = {"Authorization": await _authHeader()};

    final response =
        // await http.post(Uri.parse(uri), headers: headers, body: body);
        await http.get(Uri.parse(uri), headers: headers);

    if (response.statusCode == ok) {
      return response;
    } else {
      throw Exception('Request failed statusCode=[${response.statusCode}]');
    }
  }

  Future<List<dynamic>> query(
      String moduleName, String queryName, int startRow, endRow) async {
    debugPrint('Fetch list $moduleName.$queryName');
    return await _fetch(
            'smartlist?_operationType=fetch&_dataSource=${moduleName}_$queryName&_startRow=$startRow&_endRow=$endRow',
            '')
        .then((response) {
      debugPrint(response.body);
      return jsonDecode(response.body)['response']['data'];
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
    return await _fetch(url, '').then((response) {
      debugPrint(response.body);
      return jsonDecode(response.body)['response']['data'][0];
    });
  }

  Future<String> _authHeader() async {
/*
    var authHeader = await _storage.read(key: _authStorageKey);

    if (authHeader == null) {
      throw Exception('Stored Authorization was not found');
    }

    return authHeader;
*/
    return 'Basic ${base64.encode(utf8.encode('demo/admin:admin'))}';
  }

  /// Try to use the stored credentials to "log in", throws an error
  /// if there are no stored creds or the creds are not valid
  Future<void> tryLogin() async {
    final String uri = '${getBaseUri()}submission';
    final headers = {"Authorization": await _authHeader()};

    final response = await http.get(Uri.parse(uri), headers: headers);
    if (response.statusCode == ok) {
      return;
    }

    throw Exception('Unable to use stored credentials');
  }

  Future<Result> loginWithCredentials(
      String customer, String username, String password) async {
    String credentials = '$customer/$username:$password';
    // If not web, check if phone is connected
    if (!kIsWeb) {
      try {
        final result = await InternetAddress.lookup('example.com');
        if (result.isNotEmpty && result[0].rawAddress.isNotEmpty) {
          debugPrint('Connected');
        }
      } on SocketException catch (_) {
        debugPrint('Not Connected');
        return Result(true, 'Check your phone connectivity.');
      }
    }

    String authHeader = 'Basic ${base64.encode(utf8.encode(credentials))}';
    // Call a skyve API, and on success persist the Authorization details
    final String uri = '${getBaseUri()}submission';
    final headers = {"Authorization": authHeader};

    // FIXME need to shorten timeout, in case server is unreachable
    // currently it's 60s (I think)

    try {
      debugPrint('Attempting login with URI: $uri');

      final response = await http.get(Uri.parse(uri), headers: headers);

      if (response.statusCode == ok) {
        await _saveAuthorization(authHeader);

        return Result(false, 'no error');
      } else {
        return Result(true, 'Unknown error ${response.statusCode}',
            statusCode: response.statusCode);
      }
    } catch (ex) {
      debugPrint('Login attempt failed; $ex');
      return Result(true, ex.toString());
    }
  }

  Future<void> _saveAuthorization(String authHeaderValue) async {
    await _storage.write(key: _authStorageKey, value: authHeaderValue);
  }

  /// Clear stored credentials
  Future<void> clearAuthorization() async {
    await _storage.delete(key: _authStorageKey);
  }
}

class Result {
  late bool hasError;
  late String errorMessage;
  late int statusCode;

  Result(this.hasError, this.errorMessage, {this.statusCode = 0});
}
