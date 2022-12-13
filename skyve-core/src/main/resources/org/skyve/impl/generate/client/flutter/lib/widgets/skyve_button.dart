import 'package:flutter/material.dart';

class SkyveButton extends StatelessWidget {
  final String name;
  final String label;
  
  const SkyveButton({Key? key, required this.name, required this.label})
      : super(key: key);

  @override
  Widget build(BuildContext context) {
    if (name == 'Cancel') {
      return ElevatedButton(
          child: Text(label),
          onPressed: () {
            if (Navigator.canPop(context)) {
              Navigator.pop(context);
            }
          });
    } else {
      return ElevatedButton(child: Text(label), onPressed: () => debugPrint('Pressed button $label; action $name'));
    }
  }
}
