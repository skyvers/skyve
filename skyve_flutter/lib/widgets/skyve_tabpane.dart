import 'package:autoscale_tabbarview/autoscale_tabbarview.dart';
import 'package:flutter/material.dart';
import 'package:skyve_flutter/util/responsive_grid.dart';
import 'package:skyve_flutter/widgets/skyve_tab.dart';
import 'package:skyve_flutter/widgets/skyve_vbox.dart';

class SkyveTabPane extends StatefulWidget {
  final List<SkyveTab> tabs;

  const SkyveTabPane({super.key, required this.tabs});

  @override
  State<SkyveTabPane> createState() => _SkyveTabPaneState();
}

class _SkyveTabPaneState extends State<SkyveTabPane>
    with TickerProviderStateMixin {
  late TabController _tabController;

  @override
  void initState() {
    super.initState();
    _tabController = TabController(length: widget.tabs.length, vsync: this);
  }

  @override
  void dispose() {
    super.dispose();
    _tabController.dispose();
  }

  @override
  Widget build(BuildContext context) {
    return Column(
      children: [tabBar(), tabView()],
    );
  }

  TabBar tabBar() {
    List<Tab> tabButtons = List.generate(widget.tabs.length, (index) {
      return Tab(text: widget.tabs[index].title);
    }, growable: false);

    return TabBar(
        controller: _tabController,
        indicatorColor: Colors.orange,
        labelColor: Colors.orange,
        unselectedLabelColor: Colors.black54,
        isScrollable: true,
        tabs: tabButtons);
  }

  AutoScaleTabBarView tabView() {
    List<Padding> tabPanes = List.generate(widget.tabs.length, (index) {
      return Padding(
        padding: const EdgeInsets.all(ResponsiveWidth.defaultPadding),
        child: SkyveVBox(
          children: [widget.tabs[index]],
        ),
      );
    }, growable: false);

    return AutoScaleTabBarView(
// Stops swiping between tabs
//      physics: kIsWeb ? const NeverScrollableScrollPhysics() : null,
        controller: _tabController,
        children: tabPanes);
  }
}
