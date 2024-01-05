import 'dart:convert';

import 'package:cookie_jar/cookie_jar.dart';
import 'package:dio/dio.dart';
import 'package:dio_cookie_manager/dio_cookie_manager.dart';
import 'package:flutter/foundation.dart';

import '../models/payload.dart';
import 'package:pretty_dio_logger/pretty_dio_logger.dart';

class SkyveRestClient {
  // if served from the web, use host/ or host/context/
  // NB change this to use chrome (web) for running or debugging
  // static final String _baseUri = (kIsWeb
  //     ? (Uri.base.origin +
  //         (Uri.base.pathSegments.isEmpty
  //             ? '/'
  //             : '/${Uri.base.pathSegments[0]}/'))
  //     : 'http://localhost:8080/skyve/');
  static const baseUri = 'http://localhost:8080/skyve/';

  static const csrfHeader = 'x-csrf-token';
  static const conversationIdKey = '_c';

  static final instance = SkyveRestClient._internal();
  bool _loggedIn = false;

  final Dio _dio =
      Dio(BaseOptions(connectTimeout: const Duration(seconds: 60)));

  factory SkyveRestClient() {
    return instance;
  }

  SkyveRestClient._internal() {
    debugPrint('BaseURI is $baseUri');
    // Manage cookies
    if (!kIsWeb) {
      final cookieJar = CookieJar();
      _dio.interceptors.add(CookieManager(cookieJar));
    }

    // Log requests
    _dio.interceptors.add(PrettyDioLogger(
        requestHeader: true,
        requestBody: true,
        responseBody: true,
        responseHeader: true,
        error: true,
        compact: false,
        maxWidth: 90));
  }

  bool get loggedIn {
    return _loggedIn;
  }

  Future<String> _fetch(String url) async {
    final Response<String> response = await _dio.get(baseUri + url);
    if (response.data == null) {
      throw Exception('Response data was null');
    }

    return response.data!.replaceAll('\\n', '\\\\n').replaceAll('\t', '\\t');
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
      Response<String> response = await _dio.post('${baseUri}loginAttempt',
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
    } on DioException catch (err) {
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

  String dataSource(String m, String? d, String q) {
    return (d == null) ? '${m}_$q' : '${m}_${d}__$q';
  }

  Future<List<dynamic>> fetchQuery(
      String m, String? d, String q, int startRow, int endRow) async {
    return fetchDataSource(dataSource(m, d, q), startRow, endRow);
  }

  Future<List<dynamic>> fetchDataSource(
      String ds, int startRow, int endRow) async {
    debugPrint('Fetch list $ds');

    return await _fetch(
            'smartlist?_operationType=fetch&_dataSource=$ds&_startRow=$startRow&_endRow=$endRow')
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

  Future<Map<String, dynamic>> view(String m, String d) async {
    debugPrint('Fetch view');

    return await _fetch('meta?_mod=$m&_doc=$d&_t=true').then((jsonString) {
      debugPrint(jsonString);
      return jsonDecode(jsonString);
    });
  }

  Future<Payload> edit(
      String moduleName, String documentName, String? bizId) async {
    debugPrint('Edit bean $moduleName.$documentName#$bizId');

    String url =
        'smartedit?_operationType=fetch&_mod=$moduleName&_doc=$documentName&_ecnt=0&_ccnt=0';

    if (bizId != null) {
      url += '&bizId=$bizId';
    }

    final Response<String> response = await _dio.get(baseUri + url);
    if (response.data == null) {
      throw Exception('Response data was null');
    }

    // This is partially duplicating _fetch
    // Parse the response into a BeanContainer
    // so we have somewhere to put metadata
    Map<String, dynamic> jsonData =
        jsonDecode(response.data!)['response']['data'][0];

    String csrfToken = response.headers[csrfHeader]![0];
    Payload result = Payload(
        bizId: jsonData['bizId'],
        moduleName: moduleName,
        documentName: documentName,
        values: jsonData,
        csrfToken: csrfToken,
        conversationId: jsonData[conversationIdKey]);

    return result;
  }

  Future<Payload> addOrUpdate(String actionName, Payload requestBean) async {
    String url = 'smartedit';

    // Assemble the form data
    Map<String, String> formData = {};

    // Dynamic from action
    formData['_a'] = actionName;
    formData['_operationType'] = 'update';

    // Required by server, but do nothing
    formData['_ccnt'] = '0';
    formData['_ecnt'] = '0';

    // Dynamic values from bean
    formData['_csrf'] = requestBean.csrfToken;
    formData['_mod'] = requestBean.moduleName;
    formData['_doc'] = requestBean.documentName;
    formData[conversationIdKey] = requestBean.conversationId;
    formData['bizId'] = requestBean.bizId;

    // Servlet expects form data under key 'bean'
    // as json string
    String beanJson = jsonEncode(requestBean.values);
    formData['bean'] = beanJson;

    Response<String> response = await _postForm(url, formData);

    return _fromSkyveResponse(response, requestBean);
  }

  Payload _fromSkyveResponse(Response<String> response, Payload requestBean) {
    String csrfToken = response.headers.value(csrfHeader) ?? '-6666';

    if (response.data == null) {
      throw RestClientException('Response body was missing');
    }

    Map<String, dynamic> jsonBody = jsonDecode(response.data!);
    var jsonResponseObj = jsonBody['response'];
    int status = jsonResponseObj['status'] ?? -9999;

    if (status == 0) {
      // success response
      Map<String, dynamic> responseData = jsonResponseObj['data'];

      return Payload(
          moduleName: requestBean.moduleName,
          documentName: requestBean.documentName,
          bizId: responseData['bizId'],
          values: responseData,
          csrfToken: csrfToken,
          conversationId: responseData[conversationIdKey]);
    } else {
      // error response

      _logWarn(
          'Got error response $status for $requestBean: ${jsonResponseObj['data']}');

      var bc = Payload(
          moduleName: requestBean.moduleName,
          documentName: requestBean.documentName,
          bizId: requestBean.bizId,
          values: requestBean.values,
          csrfToken: csrfToken,
          conversationId: requestBean.conversationId);

      Map errors = jsonResponseObj['errors'] ?? {};
      bc.errors = _convertErrorsResponse(errors);
      bc.status = status;

      return bc;
    }
  }

  void _logWarn(String msg) {
    // TODO bring in a logging lib
    debugPrint('WARN: $msg');
  }

  Map<String, String> _convertErrorsResponse(Map<dynamic, dynamic> errorsIn) {
    return errorsIn.map((key, value) =>
        MapEntry(key?.toString() ?? '', value?.toString() ?? ''));
  }

  Future<Response<String>> _postForm(
      String url, Map<String, dynamic> formDataMap) {
    Options options = Options(contentType: Headers.formUrlEncodedContentType);
    return _dio.post(baseUri + url, data: formDataMap, options: options);
  }

  static String contentUrl(
      {required String module,
      required String document,
      required String binding,
      required String contentId}) {
    Map<String, String> params = {
      '_doc': '$module.$document',
      '_b': binding,
      '_n': contentId,
    };

    String paramString = Uri(queryParameters: params).query;

    return '${baseUri}content?$paramString';
  }

  static String contentImageUrl(
      {required String module,
      required String document,
      required String binding,
      required String contentId,
      required int width,
      required int height}) {
    return '${baseUri}content?_n=$contentId&_doc=$module.$document&_b=$binding&_w=$width&_h=$height';
  }
}

class RestClientException implements Exception {
  final String message;
  final Map<String, Object> details;

  RestClientException(this.message, {this.details = const {}});

  @override
  String toString() {
    return "RestClientException: $message";
  }
}
