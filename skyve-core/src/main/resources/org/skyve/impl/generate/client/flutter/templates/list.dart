import 'package:##PROJECT##/util/skyve_rest_client.dart';
import 'package:##PROJECT##/views/##MODULE##/##EDIT_DART##.dart';
import 'package:##PROJECT##/widgets/skyve_view.dart';
import 'package:flutter/material.dart';

class ##CLASS## extends StatefulWidget {
  static const routeName = '/##MODULE##/##VIEW##';

  const ##CLASS##({Key? key}) : super(key: key);

  @override
  State<StatefulWidget> createState() {
    return _##CLASS##State();
  }
}

class _##CLASS##State extends State<##CLASS##> {
  bool _loaded = false;
  List<dynamic> _rows = [];

  @override
  void initState() {
    super.initState();
    _load();
  }

  @override
  Widget build(BuildContext context) {
    return SkyveView.responsiveView(
        context,
        ##CLASS##.routeName,
        '##MODULE####QUERY##',
        Visibility(
            visible: _loaded,
            replacement: const Center(child: CircularProgressIndicator()),
            child: ListView.builder(
                itemCount: _rows.length,
                itemBuilder: (context, index) {
                  return ListTile(
                      title: Text(_rows[index]['bizId']),
                      onTap: () {
                        Navigator.pushNamed(context, ##EDIT_CLASS##.routeName,
                            arguments: _rows[index]['bizId']);
                      });
                })));
  }

  void _load() async {
    final rows = await SkyveRestClient().query('##MODULE##', '##QUERY##', 0, 75);
    setState(() {
      _rows = rows;
      _loaded = true;
    });
  }
}
