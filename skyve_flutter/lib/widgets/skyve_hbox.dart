import 'package:flutter/material.dart';
import 'package:flutter_bootstrap/flutter_bootstrap.dart';

class SkyveHBox extends StatelessWidget {
  final List<Widget> children;

  const SkyveHBox({Key? key, required this.children}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    List<BootstrapCol> cols = List.empty(growable: true);

    int length = children.length;
    int colSize = (12 / length).floor();
    String sizeString = "col-12 col-md-$colSize";

    for (Widget child in children) {
      final BootstrapCol col =
          BootstrapCol(absoluteSizes: false, sizes: sizeString, child: child);
      cols.add(col);
    }
    return BootstrapContainer(children: [BootstrapRow(children: cols)]);
  }
}
