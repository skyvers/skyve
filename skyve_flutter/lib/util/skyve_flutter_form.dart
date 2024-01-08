import 'package:flutter/widgets.dart';
import '../models/payload.dart';

class SkyveFlutterForm extends StatefulWidget {
  const SkyveFlutterForm({super.key, required this.child});

  final Widget child;

  static SkyveFlutterFormState? maybeOf(BuildContext context) {
    return context
        .dependOnInheritedWidgetOfExactType<_SkyveFlutterFormScope>()
        ?.formState;
  }

  static SkyveFlutterFormState of(BuildContext context) {
    final SkyveFlutterFormState? result = maybeOf(context);
    assert(result != null, 'No SkyveFlutterFormState found in context');
    return result!;
  }

  @override
  State<StatefulWidget> createState() {
    return SkyveFlutterFormState();
  }
}

class SkyveFlutterFormState extends State<SkyveFlutterForm> {
  /// Keep track of applied payloads, mostly for debugging
  /// though we will need the last CSRF and conversation ID
  /// later, so this might come in handy
  final _payloadHistory = <Payload>[];

  Map<String, String> metadata = {};
  Map<String, dynamic> beanValues = {};
  Map<String, String> serverErrors = {};
  int _generation = 0;

  bool get actionInProgres => false;

  String get documentName => _payloadHistory.last.documentName;

  String get moduleName => _payloadHistory.last.moduleName;

  @override
  Widget build(BuildContext context) {
    return _SkyveFlutterFormScope(
      generation: _generation,
      formState: this,
      child: widget.child,
    );
  }

  void replaceBeanValues(Map<String, dynamic> bv) {
    setState(() {
      beanValues = bv;
      _generation++;
    });
  }

  void replaceErrors(Map<String, String> errorsMap) {
    setState(() {
      serverErrors = errorsMap;
      _generation++;
    });
  }

  void removeError(String propertyKey) {
    setState(() {
      serverErrors.remove(propertyKey);
      _generation++;
    });
  }

  /// Apply the contents of the given Payload into this
  /// SkyveFormState instance
  void applyPayload(Payload payload) {
    _payloadHistory.add(payload);

    if (payload.errors.isNotEmpty) {
      replaceErrors(payload.errors);
    }

    if (payload.values.isNotEmpty) {
      replaceBeanValues(payload.values);
    }
  }

  Payload asPayload() {
    Payload lastPayload = _payloadHistory.last;

    return Payload(
        moduleName: lastPayload.moduleName,
        documentName: lastPayload.documentName,
        bizId: lastPayload.bizId,
        values: Map.of(beanValues),
        csrfToken: lastPayload.csrfToken,
        conversationId: lastPayload.conversationId);
  }
}

class _SkyveFlutterFormScope extends InheritedWidget {
  final int _generation;
  final SkyveFlutterFormState _formState;

  const _SkyveFlutterFormScope({
    required super.child,
    required generation,
    required formState,
  })  : _formState = formState,
        _generation = generation;

  SkyveFlutterFormState get formState => _formState;

  @override
  bool updateShouldNotify(_SkyveFlutterFormScope oldWidget) {
    return _generation != oldWidget._generation;
  }
}
