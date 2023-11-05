class Payload {
  Map<String, dynamic> values;
  String csrfToken;
  String bizId;
  String moduleName;
  String documentName;
  String conversationId;
  Map<String, String> errors = {};
  int status = 0;

  // title?
  // value maps

  Payload(
      {required this.moduleName,
      required this.documentName,
      required this.bizId,
      required this.values,
      required this.csrfToken,
      required this.conversationId});

  bool get successful => status == 0;

  @override
  String toString() {
    return "BeanContainer($moduleName/$documentName#$bizId)";
  }
}
