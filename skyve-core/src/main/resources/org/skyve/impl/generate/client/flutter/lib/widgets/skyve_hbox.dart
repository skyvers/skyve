import 'package:flutter/material.dart';
import 'package:flutter_bootstrap/flutter_bootstrap.dart';

class SkyveHBox extends StatelessWidget {
  final List<Widget> children;

  const SkyveHBox({Key? key, required this.children}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    List<BootstrapCol> cols = List.empty(growable: true);
    for (Widget child in children) {
      final BootstrapCol col = BootstrapCol(
          absoluteSizes: false,
          sizes: "col-12 col-md-3",
          child: child);
      cols.add(col);
    }
    return BootstrapContainer(children: [BootstrapRow(children: cols)]);
  }
}
