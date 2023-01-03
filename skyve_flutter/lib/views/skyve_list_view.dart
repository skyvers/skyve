import 'package:flutter/material.dart';
import 'package:go_router/go_router.dart';
import '../util/skyve_rest_client.dart';
import '../widgets/skyve_view.dart';

class SkyveListView extends StatefulWidget {
  final Map<String, String> queryParams;

  const SkyveListView({Key? key, this.queryParams = const {}})
      : super(key: key);

  @override
  State<StatefulWidget> createState() {
    return _SkyveListViewState();
  }
}

class _SkyveListViewState extends State<SkyveListView> {
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
        '/admin/qContacts',
        'All Contact Details',
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
                                title: Text(nvl(_rows[index]['name'])),
                                subtitle:
                                    Text(nvl(_rows[index]['contactType'])),
                                trailing: const Icon(Icons.chevron_right),
                                onTap: () {
                                  context.push(Uri(
                                      path: '/admin/Contact',
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
                      context.push('/admin/Contact');
                    }),
              )
            ])));
  }

  void _load() async {
    final rows = await SkyveRestClient().query('admin', 'qContacts', 0, 75);
    setState(() {
      _rows = rows;
      _loaded = true;
    });
  }
}
