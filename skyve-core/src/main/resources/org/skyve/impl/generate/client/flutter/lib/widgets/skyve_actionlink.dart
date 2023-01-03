import 'package:flutter/material.dart';
import 'skyve_textField.dart';

class SkyveActionLink extends StatelessWidget {
  final String label;

  const SkyveActionLink({super.key, required this.label});

  @override
  Widget build(BuildContext context) {
    // TODO: implement widget
    return SkyveTextField(label: label);
  }
}
