import 'package:flutter/material.dart';
import 'package:skyve_flutter/views/skyve_list_view.dart';
import '../util/skyve_rest_client.dart';
import '../widgets/skyve_view.dart';
import '../widgets/skyve_border.dart';
import '../widgets/skyve_button.dart';
import '../widgets/skyve_combo.dart';
import '../widgets/skyve_contentimage.dart';
import '../widgets/skyve_form.dart';
import '../widgets/skyve_formitem.dart';
import '../widgets/skyve_formrow.dart';
import '../widgets/skyve_hbox.dart';
import '../widgets/skyve_label.dart';
import '../widgets/skyve_textfield.dart';
import '../widgets/skyve_toolbar.dart';

class SkyveContainer extends StatefulWidget {
  const SkyveContainer({Key? key}) : super(key: key);

  @override
  State<StatefulWidget> createState() {
    return _SkyveContainerState();
  }
}

class _SkyveContainerState extends State<SkyveContainer> {
  dynamic _metadata;

  @override
  void initState() {
    super.initState();
    _load();
  }

  @override
  Widget build(BuildContext context) {
    if (_metadata == null) {
      return const Center(child: CircularProgressIndicator());
    }
    return const SkyveListView();
  }

  void _load() async {
    final metadata = await SkyveRestClient().metadata();
    setState(() {
      _metadata = metadata;
    });
  }
}
