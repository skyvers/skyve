class BeanContainer {
  Map<String, dynamic> values;
  String csrfToken;
  String bizId;
  String moduleName;
  String documentName;
  String conversationId;
  Map<String, String> errors = {};
  int status = 0;

  BeanContainer(
      {required this.moduleName,
      required this.documentName,
      required this.bizId,
      required this.values,
      required this.csrfToken,
      required this.conversationId});

  BeanContainer.loading()
      : values = {'_title': 'Loading'},
        csrfToken = "loading",
        bizId = "loading",
        moduleName = "loading",
        documentName = "loading",
        conversationId = "loading";

  bool get successful => status == 0;

  bool get loaded => bizId != "loading";

  @override
  String toString() {
    return "BeanContainer($moduleName/$documentName#$bizId)";
  }
}