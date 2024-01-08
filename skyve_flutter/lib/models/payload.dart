class Payload {
  final Map<String, dynamic> values;
  final String csrfToken;
  final String bizId;
  final String moduleName;
  final String documentName;
  final String conversationId;
  Map<String, String> errors = {};
  int status = 0;
  late final String title;
  // title?
  // value maps

  Payload(
      {required this.moduleName,
      required this.documentName,
      required this.bizId,
      required this.values,
      required this.csrfToken,
      required this.conversationId}) {
    title = values['_title'];
  }

  bool get successful => status == 0;

  @override
  String toString() {
    return "Payload($moduleName.$documentName#$bizId)";
  }
}
