import 'package:flutter/material.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:skyve_flutter/widgets/skyve_contentsignature.dart';

void main() {
  testWidgets('content signature uses mobile-friendly default size',
      (WidgetTester tester) async {
    await tester.pumpWidget(MaterialApp(
      home: Scaffold(
        body: SkyveContentSignature(label: 'Customer'),
      ),
    ));

    expect(tester.getSize(find.byType(SizedBox)), const Size(350, 175));
  });

  testWidgets('content signature honours explicit size',
      (WidgetTester tester) async {
    await tester.pumpWidget(MaterialApp(
      home: Scaffold(
        body: SkyveContentSignature(
            label: 'Customer', pixelWidth: 420, pixelHeight: 210),
      ),
    ));

    expect(tester.getSize(find.byType(SizedBox)), const Size(420, 210));
  });
}
