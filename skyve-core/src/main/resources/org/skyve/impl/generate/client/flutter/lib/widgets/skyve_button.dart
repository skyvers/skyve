import 'package:flutter/material.dart';
import 'package:go_router/go_router.dart';

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
            final GoRouter router = GoRouter.of(context);
            if (router.canPop()) {
              router.pop();
            }
          });
    } else {
      return ElevatedButton(
          child: Text(label),
          onPressed: () => ScaffoldMessenger.of(context).showSnackBar(SnackBar(
              duration: const Duration(seconds: 1),
              content: SizedBox(
                  height: 50.0,
                  child: Center(
                      child: Text('Pressed button $label; action $name'))))));
    }
  }
}
