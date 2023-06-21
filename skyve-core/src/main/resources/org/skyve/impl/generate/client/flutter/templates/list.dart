import 'package:##PROJECT##/util/skyve_rest_client.dart';
import 'package:##PROJECT##/views/##MODULE##/##EDIT_DART##.dart';
import 'package:##PROJECT##/widgets/skyve_view.dart';
import 'package:flutter/material.dart';
import 'package:go_router/go_router.dart';

class ##CLASS## extends StatefulWidget {
  static const routeName = '/##MODULE##/##VIEW##';

  final Map<String,String> queryParams;

  const ##CLASS##({Key? key, this.queryParams = const {}}) : super(key: key);

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
    const delete = SizedBox(
      width: 45.0,
      height: 70.0,
      child:
          Center(child: Icon(Icons.delete, color: Colors.white, size: 40.0)));

    return SkyveView.responsiveView(
        context,
        ##CLASS##.routeName,
        '##DESCRIPTION##',
        Visibility(
            visible: _loaded,
            replacement: const Center(child: CircularProgressIndicator()),
            child: Stack(children: [
              ListView.builder(
                  itemCount: _rows.length,
                  itemBuilder: (context, index) {
                    return Dismissible(
                        key: Key(_rows[index]['bizId']),
                        background: Container(
                            color: Colors.red,
                            child: Wrap(
                                alignment: WrapAlignment.spaceBetween,
                                crossAxisAlignment: WrapCrossAlignment.center,
                                children: const [delete, delete])),
                        onDismissed: (direction) {
                          ScaffoldMessenger.of(context).showSnackBar(
                              const SnackBar(
                                  content: SizedBox(
                                      height: 50.0,
                                      child: Center(child: Text('Deleted')))));
                        },
                        child: Card(
                            child: ListTile(
                                title: Text(nvl(_rows[index]['##COLUMN1##'])),
                                subtitle: Text(nvl(_rows[index]['##COLUMN2##'])),
                                trailing: const Icon(Icons.chevron_right),
                                onTap: () {
                                  context.push(Uri(
                                    path: ##EDIT_CLASS##.routeName,
                                    queryParameters: {
                                      'bizId': _rows[index]['bizId']
                                    }).toString());
                                })));
                  }),
              Positioned(
                bottom: 40.0,
                right: 40.0,
                child: FloatingActionButton(
                    child: const Icon(Icons.add),
                    onPressed: () {
                      context.push(##EDIT_CLASS##.routeName);
                    }),
              )
            ])));
  }

  void _load() async {
    final rows = await SkyveRestClient().query('##MODULE##', '##QUERY##', 0, 75);
    setState(() {
      _rows = rows;
      _loaded = true;
    });
  }
}
