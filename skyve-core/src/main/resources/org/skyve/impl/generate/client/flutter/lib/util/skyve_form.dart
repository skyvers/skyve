import 'package:flutter/widgets.dart';

class SkyveForm extends StatefulWidget {
  const SkyveForm({super.key, required this.child});

  final Widget child;

  static SkyveFormState? maybeOf(BuildContext context) {
    return context
        .dependOnInheritedWidgetOfExactType<_SkyveFormScope>()
        ?.formState;
  }

  static SkyveFormState of(BuildContext context) {
    final SkyveFormState? result = maybeOf(context);
    assert(result != null, 'No BeanContainer found in context');
    return result!;
  }

  @override
  State<StatefulWidget> createState() {
    return SkyveFormState();
  }
}

class SkyveFormState extends State<SkyveForm> {
  Map<String, String> metadata = {};
  Map<String, dynamic> beanValues = {};
  Map<String, String> serverErrors = {};
  int _generation = 0;

  bool get actionInProgres => false;

  @override
  Widget build(BuildContext context) {
    return _SkyveFormScope(
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
}

class _SkyveFormScope extends InheritedWidget {
  final int _generation;
  final SkyveFormState _formState;

  const _SkyveFormScope({
    super.key,
    required super.child,
    required generation,
    required formState,
  })  : _formState = formState,
        _generation = generation;

  SkyveFormState get formState => _formState;

  @override
  bool updateShouldNotify(_SkyveFormScope oldWidget) {
    return _generation != oldWidget._generation;
  }
}
