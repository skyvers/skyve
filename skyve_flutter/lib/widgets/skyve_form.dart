import 'package:flutter/material.dart';
import 'package:skyve_flutter/util/responsive_grid.dart';
import 'package:skyve_flutter/util/skyve_mixins.dart';
import 'package:skyve_flutter/widgets/skyve_border.dart';
import 'skyve_formcolumn.dart';
import 'skyve_formitem.dart';
import 'skyve_formrow.dart';

class SkyveForm extends StatelessWidget with Sizable, Bordered {
  final List<SkyveFormColumn> formCols;
  final List<SkyveFormRow> formRows;

  SkyveForm(
      {super.key,
      required this.formCols,
      required this.formRows,
      int? pixelWidth,
      int? responsiveWidth,
      int? percentageWidth,
      int? sm,
      int? md,
      int? lg,
      int? xl,
      int? minPixelWidth,
      int? maxPixelWidth,
      int? pixelHeight,
      int? percentageHeight,
      int? minPixelHeight,
      int? maxPixelHeight,
      bool? border,
      String? borderTitle}) {
    // Sizable
    this.pixelWidth = pixelWidth;
    this.responsiveWidth = responsiveWidth;
    this.percentageWidth = percentageWidth;
    this.sm = sm;
    this.md = md;
    this.lg = lg;
    this.xl = xl;
    this.minPixelWidth = minPixelWidth;
    this.maxPixelWidth = maxPixelWidth;

    this.pixelHeight = pixelHeight;
    this.percentageHeight = percentageHeight;
    this.minPixelHeight = minPixelHeight;
    this.maxPixelHeight = maxPixelHeight;

    // Bordered
    this.border = border ?? false;
    this.borderTitle = borderTitle;
  }

  @override
  Widget build(BuildContext context) {
    List<Widget> rows = List.empty(growable: true);

    _ResponsiveFormGrid rfg = _ResponsiveFormGrid(formCols);

    int formRowIndex = 0;
    for (SkyveFormRow formRow in formRows) {
      List<SkyveFormItem> formItems = formRow.formItems;

      rfg.reset();
      List<ResponsiveItem> responsiveItems = List.empty(growable: true);
      int formItemIndex = 0;
      for (SkyveFormItem formItem in formItems) {
        ResponsiveWidth width = rfg.getWidth(formItem.colspan);
        // padding applied to all items but the first
        EdgeInsets? padding;
        if (formItemIndex > 0) {
          padding = width.willBreak()
              ? const EdgeInsets.only(top: ResponsiveWidth.defaultPadding)
              : const EdgeInsets.only(left: ResponsiveWidth.defaultPadding);
        }
        ResponsiveItem responsiveItem = ResponsiveItem(
          width: width,
          padding: padding,
          child: formItem,
        );
        responsiveItems.add(responsiveItem);
        formItemIndex++;
      }

      // padding applied to all rows but the first
      EdgeInsets? padding;
      if (formRowIndex > 0) {
        padding = const EdgeInsets.only(top: ResponsiveWidth.defaultPadding);
      }
      ResponsiveRow responsiveRow =
          ResponsiveRow(padding: padding, children: responsiveItems);

      rows.add(responsiveRow);
      formRowIndex++;
    }

    Widget result = ResponsiveGrid(children: rows);
    if (border) {
      result = SkyveBorder(title: borderTitle, child: result);
    }
    return result;
  }
}

class _ResponsiveFormGrid {
  // The current column to be filled in the next getStyle() call.
  int _currentColumn = 0;
  // The column styles derived from the edit view markup.
  late final List<ResponsiveWidth> _columnStyles;

  _ResponsiveFormGrid(List<SkyveFormColumn> formColumns) {
    _columnStyles = List.empty(growable: true);

    // max number of columns
    int mediumColsRemaining = ResponsiveWidth.maxResponsiveWidthColumns;
    int largeColsRemaining = ResponsiveWidth.maxResponsiveWidthColumns;

    int unsizedCols = 0;

    for (SkyveFormColumn formColumn in formColumns) {
      int? pixelWidth = formColumn.pixelWidth;
      int? responsiveWidth = formColumn.responsiveWidth;
      int? sm = formColumn.sm;
      int? md = formColumn.md;
      int? lg = formColumn.lg;
      int? xl = formColumn.xl;
      int? percentageWidth = formColumn.percentageWidth;

      int small = ResponsiveWidth.maxResponsiveWidthColumns;
      int medium = 0;
      int large = 0;
      int extraLarge = 0;
      bool sized = true;

      if (responsiveWidth != null) {
        medium = responsiveWidth;
        large = medium;
        extraLarge = medium;
      } else if (pixelWidth != null) {
        medium = ResponsiveWidth.pixelWidthToMediumResponsiveWidth(
            pixelWidth.toDouble());
        large = ResponsiveWidth.pixelWidthToLargeResponsiveWidth(
            pixelWidth.toDouble());
        extraLarge = large;
      } else if (percentageWidth != null) {
        medium = ResponsiveWidth.percentageWidthToResponsiveWidth(
            percentageWidth.toDouble());
        large = medium;
        extraLarge = medium;
      } else {
        sized = false;
        unsizedCols++;
      }

      if (sized) {
        if (sm != null) {
          small = sm;
        }
        if (md != null) {
          medium = md;
        }
        if (lg != null) {
          large = lg;
        }
        if (xl != null) {
          extraLarge = xl;
        }

        mediumColsRemaining -= medium;
        largeColsRemaining -= large;
        _columnStyles.add(ResponsiveWidth(small, medium, large, extraLarge));
      } else {
        _columnStyles.add(ResponsiveWidth(-1, -1, -1, -1)); // unsized
      }
    }

    // Take care of unsized columns
    if (unsizedCols > 0) {
      int medium = (mediumColsRemaining / unsizedCols).floor();
      int large = (largeColsRemaining / unsizedCols).floor();

      for (ResponsiveWidth style in _columnStyles) {
        if (style.sm == -1) {
          style.sm = ResponsiveWidth.maxResponsiveWidthColumns;
          style.md = medium;
          style.lg = large;
          style.xl = large;
        }
      }
    }
  }

  ResponsiveWidth getWidth(int colspan) {
    // reset the current column index if it will be out of bounds
    if (_currentColumn > (_columnStyles.length - 1)) {
      _currentColumn = 0;
    }
    // Get this column style
    ResponsiveWidth result = _columnStyles[_currentColumn++];
    // Add all the next colspan column styles
    for (int i = 1, l = colspan; i < l; i++) {
      if (_currentColumn > (_columnStyles.length - 1)) {
        _currentColumn = 0;
      }
      result = result.add(_columnStyles[_currentColumn++]);
    }

    return result;
  }

  // Reset the style.
  void reset() {
    _currentColumn = 0;
  }
}
