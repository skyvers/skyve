import 'package:flutter/material.dart';

class SkyveTextField extends StatefulWidget {
  final String label;
  final Map beanValues;
  final String propertyKey;
  final int? maxlines;

  const SkyveTextField(
      {Key? key,
      required this.label,
      required this.beanValues,
      required this.propertyKey,
      this.maxlines})
      : super(key: key);

  @override
  State<StatefulWidget> createState() => SkyveTextFieldState();
}

class SkyveTextFieldState extends State<SkyveTextField> {
  @override
  Widget build(BuildContext context) {
    return TextFormField(
      decoration: InputDecoration(
          border: const OutlineInputBorder(), labelText: widget.label),
      controller: _createController(widget.beanValues, widget.propertyKey),
      maxLines: widget.maxlines,
    );
  }

  TextEditingController _createController(Map beanValues, String key) {
    var tec = TextEditingController(text: (beanValues[key] ?? '').toString());
    tec.addListener(() {
      beanValues[key] = tec.text;
    });

    return tec;
  }
}