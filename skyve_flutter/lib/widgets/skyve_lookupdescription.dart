import 'package:flutter/material.dart';
import 'skyve_textField.dart';

class SkyveLookupDescription extends StatelessWidget {
  final String label;

  const SkyveLookupDescription({super.key, required this.label});

  @override
  Widget build(BuildContext context) {
    // TODO: implement widget
    return SkyveTextField(label: label);
  }
}
