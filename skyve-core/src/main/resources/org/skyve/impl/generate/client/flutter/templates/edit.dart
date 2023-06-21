import 'package:##PROJECT##/util/skyve_rest_client.dart';
import 'package:##PROJECT##/widgets/skyve_view.dart';
import 'package:flutter/material.dart';
import 'package:flutter_bootstrap/flutter_bootstrap.dart';
##IMPORTS##

class ##CLASS## extends StatefulWidget {
  static const routeName = '/##MODULE##/##DOCUMENT##';

  final Map<String,String> queryParams;

  const ##CLASS##({Key? key, this.queryParams = const {}}) : super(key: key);

  @override
  State<StatefulWidget> createState() {
    return _##CLASS##State();
  }
  
  String? get _bizId {
    return queryParams['bizId'];
  }
}

class _##CLASS##State extends State<##CLASS##> {
  Map<String, dynamic> _bean = {'_title': 'Loading'};

  @override
  Widget build(BuildContext context) {
    final bizId = widget._bizId;
    _load(bizId);
    return SkyveView.responsiveView(
        context,
        ##CLASS##.routeName,
        _bean['_title'],
        Visibility(
            visible: (_bean['bizId'] != null),
            replacement: const Center(child: CircularProgressIndicator()),
            child: SingleChildScrollView(
              child: ##DART##
            )
        )
    );
  }

  void _load(String? bizId) async {
    if (_bean['bizId'] == null) {
      final bean = await SkyveRestClient().edit('##MODULE##', '##DOCUMENT##', bizId);
      setState(() {
        _bean = bean;
      });
    }
  }
}
