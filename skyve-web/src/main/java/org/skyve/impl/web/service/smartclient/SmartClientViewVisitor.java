package org.skyve.impl.web.service.smartclient;

import java.util.List;
import java.util.Locale;
import java.util.Stack;

import org.skyve.domain.Bean;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.generate.SmartClientGenerateUtils;
import org.skyve.impl.generate.SmartClientGenerateUtils.SmartClientDataGridFieldDefinition;
import org.skyve.impl.generate.SmartClientGenerateUtils.SmartClientFieldDefinition;
import org.skyve.impl.generate.SmartClientGenerateUtils.SmartClientLookupDefinition;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.view.AbsoluteSize;
import org.skyve.impl.metadata.view.AbsoluteWidth;
import org.skyve.impl.metadata.view.ActionImpl;
import org.skyve.impl.metadata.view.Bordered;
import org.skyve.impl.metadata.view.ConstrainableHeight;
import org.skyve.impl.metadata.view.ConstrainableSize;
import org.skyve.impl.metadata.view.ContentSpecifiedWidth;
import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.impl.metadata.view.Inject;
import org.skyve.impl.metadata.view.LayoutUtil;
import org.skyve.impl.metadata.view.MinimumHeight;
import org.skyve.impl.metadata.view.RelativeSize;
import org.skyve.impl.metadata.view.ShrinkWrap;
import org.skyve.impl.metadata.view.ShrinkWrapper;
import org.skyve.impl.metadata.view.VerticalAlignment;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.metadata.view.ViewVisitor;
import org.skyve.impl.metadata.view.container.Box;
import org.skyve.impl.metadata.view.container.HBox;
import org.skyve.impl.metadata.view.container.Tab;
import org.skyve.impl.metadata.view.container.TabPane;
import org.skyve.impl.metadata.view.container.VBox;
import org.skyve.impl.metadata.view.container.form.Form;
import org.skyve.impl.metadata.view.container.form.FormColumn;
import org.skyve.impl.metadata.view.container.form.FormItem;
import org.skyve.impl.metadata.view.container.form.FormRow;
import org.skyve.impl.metadata.view.event.Addable;
import org.skyve.impl.metadata.view.event.Changeable;
import org.skyve.impl.metadata.view.event.Editable;
import org.skyve.impl.metadata.view.event.EventSource;
import org.skyve.impl.metadata.view.event.Focusable;
import org.skyve.impl.metadata.view.event.Removable;
import org.skyve.impl.metadata.view.event.RerenderEventAction;
import org.skyve.impl.metadata.view.event.Selectable;
import org.skyve.impl.metadata.view.event.ServerSideActionEventAction;
import org.skyve.impl.metadata.view.event.SetDisabledEventAction;
import org.skyve.impl.metadata.view.event.SetInvisibleEventAction;
import org.skyve.impl.metadata.view.event.ToggleDisabledEventAction;
import org.skyve.impl.metadata.view.event.ToggleVisibilityEventAction;
import org.skyve.impl.metadata.view.widget.Blurb;
import org.skyve.impl.metadata.view.widget.Button;
import org.skyve.impl.metadata.view.widget.DialogButton;
import org.skyve.impl.metadata.view.widget.DynamicImage;
import org.skyve.impl.metadata.view.widget.GeoLocator;
import org.skyve.impl.metadata.view.widget.Link;
import org.skyve.impl.metadata.view.widget.MapDisplay;
import org.skyve.impl.metadata.view.widget.Spacer;
import org.skyve.impl.metadata.view.widget.StaticImage;
import org.skyve.impl.metadata.view.widget.bound.Label;
import org.skyve.impl.metadata.view.widget.bound.ProgressBar;
import org.skyve.impl.metadata.view.widget.bound.input.CheckBox;
import org.skyve.impl.metadata.view.widget.bound.input.CheckMembership;
import org.skyve.impl.metadata.view.widget.bound.input.ColourPicker;
import org.skyve.impl.metadata.view.widget.bound.input.Combo;
import org.skyve.impl.metadata.view.widget.bound.input.Comparison;
import org.skyve.impl.metadata.view.widget.bound.input.ContentImage;
import org.skyve.impl.metadata.view.widget.bound.input.ContentLink;
import org.skyve.impl.metadata.view.widget.bound.input.Geometry;
import org.skyve.impl.metadata.view.widget.bound.input.HTML;
import org.skyve.impl.metadata.view.widget.bound.input.InputWidget;
import org.skyve.impl.metadata.view.widget.bound.input.ListMembership;
import org.skyve.impl.metadata.view.widget.bound.input.Lookup;
import org.skyve.impl.metadata.view.widget.bound.input.LookupDescription;
import org.skyve.impl.metadata.view.widget.bound.input.Password;
import org.skyve.impl.metadata.view.widget.bound.input.Radio;
import org.skyve.impl.metadata.view.widget.bound.input.RichText;
import org.skyve.impl.metadata.view.widget.bound.input.Slider;
import org.skyve.impl.metadata.view.widget.bound.input.Spinner;
import org.skyve.impl.metadata.view.widget.bound.input.TextArea;
import org.skyve.impl.metadata.view.widget.bound.input.TextField;
import org.skyve.impl.metadata.view.widget.bound.tabular.AbstractDataWidget;
import org.skyve.impl.metadata.view.widget.bound.tabular.AbstractListWidget;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridBoundColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridContainerColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataRepeater;
import org.skyve.impl.metadata.view.widget.bound.tabular.DisableableCRUDGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListRepeater;
import org.skyve.impl.metadata.view.widget.bound.tabular.TreeGrid;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Collection;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.DynamicImage.ImageFormat;
import org.skyve.metadata.model.document.Relation;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.DocumentQueryDefinition;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.Action;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.metadata.view.widget.bound.FilterParameter;
import org.skyve.metadata.view.widget.bound.Parameter;
import org.skyve.util.Binder.TargetMetaData;
import org.skyve.util.Util;

class SmartClientViewVisitor extends ViewVisitor {
	private static final Integer DEFAULT_MIN_HEIGHT_IN_PIXELS = Integer.valueOf(100);
	
	private User user;
	private Locale locale;
	private boolean noCreateView;
	private int variableCounter = 0;

	// This is used to assign names to boilerplate values which have binding expressions in them, such as links.
	// These values are evaluated server side and stashed in the JSON response under a bogus binding.
	private int formatCounter = 0;
	
	private StringBuilder code = new StringBuilder(2048);
	private Stack<String> containerVariables = new Stack<>();

	public SmartClientViewVisitor(User user,
									Customer customer, 
									Module module,
									Document document,
									View view,
									boolean noCreateView) {
		super((CustomerImpl) customer,
				(ModuleImpl) module, 
				(DocumentImpl) document,
				(ViewImpl) view);
		this.user = user;
		this.locale = (user == null) ? null : user.getLocale();
		this.noCreateView = noCreateView;
	}

	public StringBuilder getCode() {
		return code;
	}

	@Override
	public void visitView() {
		UtilImpl.LOGGER.info("VIEW = " + view.getTitle() + " for " + document.getName());
		if (noCreateView) {
			containerVariables.push("view");
		}
		else if (ViewType.edit.toString().equals(view.getName())) {
			code.append("var edit = isc.BizContainer.create({width:'100%',height:'100%',invisibleConditionName:'");
			code.append(Bean.NOT_CREATED_KEY);
			code.append("'});");
			containerVariables.push("edit");
		}
		else if (ViewType.create.toString().equals(view.getName())) {
			code.append("var create = isc.BizContainer.create({width:'100%',height:'100%',invisibleConditionName:'");
			code.append(Bean.CREATED_KEY);
			code.append("'});");
			containerVariables.push("create");
		}
	}

	// This is a stack in case he have a tab pane inside a tab pane
	private Stack<Integer> tabNumbers = new Stack<>();
	
	@Override
	public void visitTabPane(TabPane tabPane, 
								boolean parentVisible,
								boolean parentEnabled) {
		tabNumbers.push(Integer.valueOf(0));
		String variable = "v" + variableCounter++;
		code.append("var ").append(variable).append("=isc.BizTabPane.create({");
		size(tabPane, DEFAULT_MIN_HEIGHT_IN_PIXELS, code);
		disabled(tabPane.getDisabledConditionName(), code);
		invisible(tabPane.getInvisibleConditionName(), code);
		String selected = tabPane.getSelectedTabIndexBinding();
		if (selected != null) {
			code.append("selectedTabIndexBinding:'").append(selected).append("',");
		}
		code.append("_view:view});\n");

		containerVariables.push(variable);
	}

	@Override
	public void visitedTabPane(TabPane tabPane, 
								boolean parentVisible,
								boolean parentEnabled) {
		String variable = containerVariables.pop();
		code.append(containerVariables.peek()).append(".addContained(").append(variable).append(");\n");
		tabNumbers.pop();
	}

	@Override
	public void visitTab(Tab tab, 
							boolean parentVisible,
							boolean parentEnabled) {
		String variable = "v" + variableCounter++;
		code.append("var ").append(variable).append("=isc.BizContainer.create({membersMargin:10,layoutMargin:10});\n");

		containerVariables.push(variable);
	}

	@Override
	public void visitedTab(Tab tab, 
							boolean parentVisible,
							boolean parentEnabled) {
		String paneVariable = containerVariables.pop();
		String tabPaneVariable = containerVariables.peek();
		Integer tabNumber = tabNumbers.pop();
		code.append(tabPaneVariable).append(".addBizTab({name:'").append(tabNumber);
		String icon16 = tab.getIcon16x16RelativeFileName();
		if (icon16 != null) {
			code.append("',icon:'../resources?_doc=");
			code.append(module.getName()).append('.').append(document.getName());
			code.append("&_n=").append(icon16);
			code.append("',title:'");
		}
		else {
			code.append("',title:'");
			String iconStyleClass = tab.getIconStyleClass();
			if (iconStyleClass != null) {
				code.append("<i class=\"bizhubFontIcon ").append(iconStyleClass).append("\"></i>");
			}
		}
		code.append(SmartClientGenerateUtils.processString(Util.i18n(tab.getTitle(), locale)));
		code.append("',pane:").append(paneVariable).append(',');
		tabNumbers.push(Integer.valueOf(tabNumber.intValue() + 1));
		disabled(tab.getDisabledConditionName(), code);
		invisible(tab.getInvisibleConditionName(), code);
		removeTrailingComma(code);
		code.append("});\n");
	}

	@Override
	public void visitVBox(VBox vbox, 
							boolean parentVisible,
							boolean parentEnabled) {
		String variable = "v" + variableCounter++;
		code.append("var ").append(variable).append("=isc.BizVBox.create({");
		size(vbox, null, code);
		bordered(vbox, vbox.getPixelPadding(), code);
		box(vbox);
		VerticalAlignment v = vbox.getVerticalAlignment();
		if (v != null) {
			switch (v) {
			case top:
				code.append("align:'top',");
				break;
			case middle:
				code.append("align:'center',");
				break;
			case bottom:
				code.append("align:'bottom',");
				break;
			default:
				throw new MetaDataException("VBox VerticalAlignment of " + v + " is not supported");
			}
		}
		HorizontalAlignment h = vbox.getHorizontalAlignment();
		if (h != null) {
			switch (h) {
			case left:
				code.append("defaultLayoutAlign:'left',");
				break;
			case centre:
				code.append("defaultLayoutAlign:'center',");
				break;
			case right:
				code.append("defaultLayoutAlign:'right',");
				break;
			default:
				throw new MetaDataException("VBox HorizontalAlignment of " + h + " is not supported");
			}
		}
		invisible(vbox.getInvisibleConditionName(), code);
		removeTrailingComma(code);
		code.append("});\n");
		code.append(containerVariables.peek()).append(".addContained(").append(variable).append(");\n");

		containerVariables.push(variable);
	}

	@Override
	public void visitHBox(HBox hbox, 
							boolean parentVisible,
							boolean parentEnabled) {
		String variable = "v" + variableCounter++;
		code.append("var ").append(variable).append("=isc.BizHBox.create({");
		size(hbox, null, code);
		HorizontalAlignment h = hbox.getHorizontalAlignment();
		if (h != null) {
			switch (h) {
			case left:
				code.append("align:'left',");
				break;
			case centre:
				code.append("align:'center',");
				break;
			case right:
				code.append("align:'right',");
				break;
			default:
				throw new MetaDataException("HBox HoriaontalAlignment of " + h + " is not supported");
			}
		}
		VerticalAlignment v = hbox.getVerticalAlignment();
		if (v != null) {
			switch (v) {
			case top:
				code.append("defaultLayoutAlign:'top',");
				break;
			case middle:
				code.append("defaultLayoutAlign:'center',");
				break;
			case bottom:
				code.append("defaultLayoutAlign:'bottom',");
				break;
			default:
				throw new MetaDataException("HBox HoriaontalAlignment of " + h + " is not supported");
			}
		}
		bordered(hbox, hbox.getPixelPadding(), code);
		box(hbox);
		invisible(hbox.getInvisibleConditionName(), code);
		removeTrailingComma(code);
		code.append("});\n");
		code.append(containerVariables.peek()).append(".addContained(").append(variable).append(");\n");

		containerVariables.push(variable);
	}

	private void box(Box box) {
		Integer padding = box.getPixelPadding();
		if (padding != null) {
			code.append("layoutMargin:").append(padding).append(',');
		}
		Integer memberPadding = box.getPixelMemberPadding();
		if (memberPadding != null) {
			code.append("membersMargin:").append(memberPadding).append(',');
		}
		else {
			code.append("membersMargin:10,");
		}
	}

	private boolean viewHasAtLeastOneForm = false;
	private String formVariable = null;
	private VBox borderBox = null;
	
	@Override
	public void visitForm(Form form, 
							boolean parentVisible,
							boolean parentEnabled) {
		viewHasAtLeastOneForm = true;
		
		// If a form is defined with a border, then wrap the form definition in a vbox.
		// SC 8.2 couldn't cope with chrome and would draw the fieldset/border too small 
		// for its content.
		Boolean border = form.getBorder();
		if (Boolean.TRUE.equals(border)) {
			borderBox = new VBox();
			borderBox.setBorder(Boolean.TRUE);
			borderBox.setBorderTitle(form.getBorderTitle());
			borderBox.setInvisibleConditionName(form.getInvisibleConditionName());
			borderBox.setPixelWidth(form.getPixelWidth());
			borderBox.setResponsiveWidth(form.getResponsiveWidth());
			borderBox.setPercentageWidth(form.getPercentageWidth());
			borderBox.setPixelPadding(Integer.valueOf(10));

			// If no height is specified, use a height of 1 pixel 
			// which makes the bordered vbox expand to fit its contents
			Integer percentageHeight = form.getPercentageHeight();
			Integer pixelHeight = form.getPixelHeight();
			if ((percentageHeight == null) && (pixelHeight == null)) {
				pixelHeight = Integer.valueOf(1);
			}
			borderBox.setPercentageHeight(percentageHeight);
			borderBox.setPixelHeight(pixelHeight);

			visitVBox(borderBox, parentVisible, parentEnabled);
		}
		
		formVariable = "v" + variableCounter++;
		code.append("var ").append(formVariable);
		code.append("=isc.DynamicForm.create({longTextEditorType:'text',longTextEditorThreshold:102400,");
		// SC docs says that autoFocus will focus in first focusable item
		// in the form when it is drawn.
		// Don't use autoFocus as we have multiple dynamic forms that can be declared
		// in some views which doesn't work.
		code.append("_view:view,");
		code.append("ID:").append(IDExpression()).append(',');
		disabled(form.getDisabledConditionName(), code);
//code.append("cellBorder:1,");
		
		if (! Boolean.TRUE.equals(border)) { // false or null
			size(form, null, code);
			invisible(form.getInvisibleConditionName(), code);
		}
		
		HorizontalAlignment alignment = form.getLabelDefaultHorizontalAlignment();
		if (alignment != null) {
			code.append("titleAlign:'").append(alignment).append("',");
		}
		code.append("numCols:");
		code.append(form.getColumns().size());
		code.append(",colWidths:[");
	}

	@Override
	public void visitedForm(Form form,
								boolean parentVisible,
								boolean parentEnabled) {
		code.setLength(code.length() - 1); // remove last comma
		code.append("]);\n");
		code.append(containerVariables.peek()).append(".addContained(").append(formVariable).append(");\n");
		formVariable = null;
		visitedFormRow = false;
		
		if (Boolean.TRUE.equals(form.getBorder())) {
			visitedVBox(borderBox, parentVisible, parentEnabled);
			borderBox = null;
		}
	}

	@Override
	public void visitFormColumn(FormColumn column,
									boolean parentVisible,
									boolean parentEnabled) {
		Integer percentage = column.getPercentageWidth();
		Integer pixel = column.getPixelWidth();
		Integer responsive = column.getResponsiveWidth();
		if (pixel != null) {
			code.append(pixel).append(',');
		}
		else if (percentage != null) {
			code.append('\'').append(percentage).append("%',");
		}
		else if (responsive != null) {
			code.append('\'');
			code.append(LayoutUtil.responsiveWidthToPercentageWidth(responsive.doubleValue()));
			code.append("%',"); 
		}
		else {
			code.append("'*',");
		}
	}

	// have we visited a form row in this form yet
	private boolean visitedFormRow = false;
	// have we started a new row
	private boolean startedNewFormRow = false;

	@Override
	public void visitFormRow(FormRow row,
								boolean parentVisible,
								boolean parentEnabled) {
		startedNewFormRow = true;
		if (! visitedFormRow) {
			code.setLength(code.length() - 1); // remove last column comma
			code.append("]});");
			code.append("view._vm.addMember(").append(formVariable).append(");\n");
			code.append(formVariable).append(".setItems([");
		}

		visitedFormRow = true;
	}

	// The enclosing form item
	public FormItem visitedItem;
	
	@Override
	public void visitFormItem(FormItem item, boolean parentVisible, boolean parentEnabled) {
		visitedItem = item;
		
		code.append('{');
		Boolean showLabel = item.getShowLabel();
		if (showLabel != null) {
			code.append("showTitle:").append(showLabel).append(',');
		}
		// label handled in preProcessFormItem()
		Integer span = item.getColspan();
		if (span != null) {
			code.append("colSpan:").append(span).append(',');
		}
		span = item.getRowspan();
		if (span != null) {
			code.append("rowSpan:").append(span).append(',');
		}
		HorizontalAlignment horizontalAlignment = item.getHorizontalAlignment();
		if (horizontalAlignment != null) {
			code.append("align:'").append(horizontalAlignment.toAlignmentString()).append("',");
		}
		horizontalAlignment = item.getLabelHorizontalAlignment();
		if (horizontalAlignment != null) {
			code.append("titleAlign:'").append(horizontalAlignment.toAlignmentString()).append("',");
		}
//		item.getVerticalAlignment()
//		item.getShowHelp()
//		code.append("{width:'*',");
	}

	@Override
	public void visitedFormItem(FormItem item,
									boolean parentVisible,
									boolean parentEnabled) {
		if (startedNewFormRow) {
			code.append("startRow:true},");
			startedNewFormRow = false;
		}
		else {
			code.append("startRow:false},");
		}
		visitedItem = null;
	}

	@Override
	public void visitedFormRow(FormRow row,
								boolean parentVisible,
								boolean parentEnabled) {
		code.setLength(code.length() - 2); // remove "},"
		code.append(",endRow:true},");
	}

	@Override
	public void visitButton(Button button,
								boolean parentVisible,
								boolean parentEnabled) {
		Action action = view.getAction(button.getActionName());

		String buttonCode = generateButton(action.getResourceName(),
											action.getImplicitName(),
											action.getDisplayName(),
											action.getClientValidation(),
											action.getRelativeIconFileName(),
											action.getIconStyleClass(),
											action.getToolTip(),
											action.getConfirmationText(),
											action.getParameters(),
											action.getDisabledConditionName(),
											action.getInvisibleConditionName(),
											button);
		if (buttonCode != null) { // we have access
			if (formVariable == null) {
				String variable = "v" + variableCounter++;
				code.append("var ").append(variable).append('=').append(buttonCode).append(";\n");
				code.append(containerVariables.peek()).append(".addContained(").append(variable).append(");\n");
			}
			else {
				code.append("type:'canvas',showTitle:false,width:1,canvas:isc.HLayout.create({height:22,members:[");
				code.append(buttonCode).append("]}),");
				disabled(action.getDisabledConditionName(), code);
				invisible(action.getInvisibleConditionName(), code);
			}
		}
		else {
			if (formVariable != null) {
				code.append("type:'spacer',");
			}
		}
	}

	@Override
	public void visitGeoLocator(GeoLocator locator,
									boolean parentVisible,
									boolean parentEnabled) {
		StringBuilder geoLocatorCode = new StringBuilder(256);
		geoLocatorCode.append("isc.BizUtil.createGeoLocator(view,");
		String binding = locator.getLatitudeBinding();
		if (binding == null) {
			geoLocatorCode.append("null");
		}
		else {
			geoLocatorCode.append('\'').append(binding).append('\'');
		}
		binding = locator.getLongitudeBinding();
		if (binding == null) {
			geoLocatorCode.append(",null");
		}
		else {
			geoLocatorCode.append(",'").append(binding).append('\'');
		}
		binding = locator.getDescriptionBinding();
		if (binding == null) {
			geoLocatorCode.append(",null");
		}
		else {
			geoLocatorCode.append(",'").append(binding).append('\'');
		}
		binding = locator.getAddressBinding();
		if (binding == null) {
			geoLocatorCode.append(",null");
		}
		else {
			geoLocatorCode.append(",'").append(binding).append('\'');
		}
		binding = locator.getCityBinding();
		if (binding == null) {
			geoLocatorCode.append(",null");
		}
		else {
			geoLocatorCode.append(",'").append(binding).append('\'');
		}
		binding = locator.getStateBinding();
		if (binding == null) {
			geoLocatorCode.append(",null");
		}
		else {
			geoLocatorCode.append(",'").append(binding).append('\'');
		}
		binding = locator.getPostcodeBinding();
		if (binding == null) {
			geoLocatorCode.append(",null");
		}
		else {
			geoLocatorCode.append(",'").append(binding).append('\'');
		}
		binding = locator.getCountryBinding();
		if (binding == null) {
			geoLocatorCode.append(",null");
		}
		else {
			geoLocatorCode.append(",'").append(binding).append('\'');
		}

		if (formVariable == null) {
			String variable = "v" + variableCounter++;
			code.append("var ").append(variable).append('=').append(geoLocatorCode).append(");\n");
			code.append(containerVariables.peek()).append(".addContained(").append(variable).append(");\n");
		}
		else {
			code.append("type:'canvas',showTitle:false,canvas:isc.HLayout.create({height:22,members:[");
			code.append(geoLocatorCode).append(")]}),");
		}
	}

	@Override
	public void visitGeometry(Geometry geometry, 
								boolean parentVisible,
								boolean parentEnabled) {
		if (dataWidgetVariable != null) {
			dataWidgetColumnInputWidget = geometry;
			return;
		}

		if (formVariable == null) {
			throw new MetaDataException("Geometry found without a form");
		}

		preProcessFormItem(geometry, "geometry");
		size(geometry, null, code);
		disabled(geometry.getDisabledConditionName(), code);
		invisible(geometry.getInvisibleConditionName(), code);
		
		// Highlight text on focus
		code.append("selectOnFocus:true,");
		
		// TODO add in the filter operators allowed
	}

	@Override
	public void visitMap(MapDisplay map,
							boolean parentVisible,
							boolean parentEnabled) {
		String variable = "v" + variableCounter++;
		code.append("var ").append(variable).append("=isc.BizMap.create({_view:view});");
		code.append(variable).append(".setDataSource('").append(map.getModelName()).append("');\n");
		code.append(containerVariables.peek()).append(".addContained(").append(variable).append(");\n");
	}

	@Override
	public void visitDialogButton(DialogButton button,
									boolean parentVisible,
									boolean parentEnabled) {
		if (formVariable == null) {
			String variable = "v" + variableCounter++;
			code.append("var ").append(variable).append("=isc.BizLabel.create({value: '");
			code.append(SmartClientGenerateUtils.processString(Util.i18n(button.getCommand(), locale)));
			code.append("'});\n");
			code.append(containerVariables.peek()).append(".addContained(").append(variable).append(");\n");
		}
		else {
			code.append("type:'blurb',defaultValue:'dialog button ");
			code.append(SmartClientGenerateUtils.processString(Util.i18n(button.getCommand(), locale))).append("',");
			disabled(button.getDisabledConditionName(), code);
			invisible(button.getInvisibleConditionName(), code);
		}
	}

	@Override
	public void visitDynamicImage(DynamicImage image,
									boolean parentVisible,
									boolean parentEnabled) {
		// markup is generated in the JSON data for a data grid container column dynamic image
		if (dataWidgetVariable != null) {
			return;
		}

		if (formVariable == null) {
			String variable = "v" + variableCounter++;
			code.append("var ").append(variable).append('=');
			addImage(image);
			code.append(";\n");
			code.append(containerVariables.peek()).append(".addContained(").append(variable).append(");\n");
		}
	}

	private void addImage(DynamicImage image) {
		code.append("isc.BizDynamicImage.create({name:'");
		code.append(image.getName());
		code.append("',moduleDotDocument:'");
		code.append(document.getOwningModuleName());
		code.append('.');
		code.append(document.getName()).append("',format:'");
		ImageFormat format = document.getDynamicImage(customer, image.getName()).getFormat();
		code.append((format != null) ? format : ImageFormat.png).append("',");
		size(image, null, code);
		invisible(image.getInvisibleConditionName(), code);
		Integer initialWidth = image.getImageInitialPixelWidth();
		if (initialWidth != null) {
			code.append("imageWidth:").append(initialWidth).append(',');
		}
		Integer initialHeight = image.getImageInitialPixelHeight();
		if (initialHeight != null) {
			code.append("imageHeight:").append(initialHeight).append(',');
		}
		code.append("_view:view})");
	}

	// TODO size, invisibility and binding
	@Override
	public void visitStaticImage(StaticImage image,
									boolean parentVisible,
									boolean parentEnabled) {
		// markup is generated in the JSON data for a data grid container column static image
		if (dataWidgetVariable != null) {
			return;
		}

		if (formVariable == null) {
			String variable = "v" + variableCounter++;
			code.append("var ").append(variable).append('=');
			addStaticImage(image);
			code.append(";\n");
			code.append(containerVariables.peek()).append(".addContained(").append(variable).append(");\n");
		}
		else {
			code.append("type:'canvas',showTitle:false,canvas:");
			addStaticImage(image);
			code.append(',');
		}
	}

	private void addStaticImage(StaticImage image) {
		code.append("isc.BizImage.create({modoc:'").append(module.getName()).append('.').append(document.getName());
		code.append("',file:'").append(image.getRelativeFile()).append("',");
		size(image, null, code);
		removeTrailingComma(code);
		code.append("})");
	}

	@Override
	public void visitSpacer(Spacer spacer) {
		if (formVariable == null) { // not a form
			String variable = "v" + variableCounter++;
			code.append("var ").append(variable).append("=isc.LayoutSpacer.create(");
	        if ((spacer.getPixelWidth() != null) || 
	        		(spacer.getPixelHeight() != null) ||
	        		(spacer.getInvisibleConditionName() != null)) {
	        	code.append('{');
	        	size(spacer, null, code);
		        invisible(spacer.getInvisibleConditionName(), code);
	        	code.setLength(code.length() - 1); // remove trailing comma
	        	code.append('}');
	        }
			code.append(");\n");
			code.append(containerVariables.peek()).append(".addContained(").append(variable).append(");\n");
		}
		else {
			code.append("type:'spacer',");
			size(spacer, null, code);
	        invisible(spacer.getInvisibleConditionName(), code);
		}
	}

	@Override
	public void visitLink(Link link, 
							boolean parentVisible, 
							boolean parentEnabled) {
		// markup is generated in the JSON data for a data grid container column link
		if (dataWidgetVariable != null) {
			return;
		}

		if (formVariable == null) {
			// TODO fix this later
		}
		else {
			// Take care of the title, as we're not calling preProcessFormItem
			String label = visitedItem.getLabel();
			if (label == null) {
				label = "Link";
			}
			label = SmartClientGenerateUtils.processString(Util.i18n(label, locale));
			code.append("title:'").append(label).append("',");
			code.append("type:'blurb',name:'_");
			code.append(formatCounter++).append("',"); // _1, _2 and so on
			size(link, null, code);
			invisible(link.getInvisibleConditionName(), code);
		}
	}

	@Override
	public void visitBlurb(Blurb blurb,
							boolean parentVisible,
							boolean parentEnabled) {
		Label label = new Label();
		label.setValue(blurb.getMarkup());
		label.setPixelWidth(blurb.getPixelWidth());
		label.setPixelHeight(blurb.getPixelHeight());
		label.setTextAlignment(blurb.getTextAlignment());
		label.setInvisibleConditionName(blurb.getInvisibleConditionName());
		visitLabel(label, parentVisible, parentEnabled);
	}

	// Invisible
	@Override
	public void visitLabel(Label label,
							boolean parentVisible,
							boolean parentEnabled) {
		// markup is generated in the JSON data for a data grid container column label or a dynamic form-based value
		if (dataWidgetVariable != null) {
			return;
		}
		
		String binding = label.getBinding();
		String value = label.getValue();

		// Find the display name if applicable
		String displayName = "Label";
		String displayBinding = label.getFor();
		if (displayBinding == null) {
			displayBinding = binding;
		}
		if (displayBinding != null) {
			TargetMetaData target = BindUtil.getMetaDataForBinding(customer, module, document, displayBinding);
			if (target != null) {
				Attribute attribute = target.getAttribute();
				if (attribute != null) {
					displayName = attribute.getDisplayName();
				}
			}
		}

		// does the value have binding expressions in them? - (?s) means mutliline match
		boolean dynamic = (value != null) && BindUtil.messageIsBound(value); 
		if (dynamic) {
			if ((dataWidgetBinding == null) && (formVariable == null)) {
				throw new MetaDataException("Label or blurb with a value of [" + value + 
												"] contains a binding expression and must be declared within a form element or a data grid container column to be able to bind correctly");
			}

			value = null;
			binding = "_" + formatCounter++; // _1, _2 and so on
		}
		
		HorizontalAlignment alignment = label.getTextAlignment();

		if (formVariable == null) {
			String variable = "v" + variableCounter++;
			code.append("var ").append(variable).append("=isc.BizLabel.create({");

			size(label, null, code);
			if (label.getPixelWidth() == null) { // default to whole width
				code.append("width:'100%',");
			}

			if (alignment != null) {
				code.append("textAlign:'").append(alignment.toAlignmentString()).append("',");
			}
			
			if (binding == null) {
				code.append("value:'");
				code.append(SmartClientGenerateUtils.processString((value == null) ? Util.i18n(displayName, locale) : Util.i18n(value, locale), false, false));
			}
			else {
				code.append("binding:'").append(BindUtil.sanitiseBinding(binding));
			}
			
			code.append("'});\n");
			code.append(containerVariables.peek()).append(".addContained(").append(variable).append(");\n");
		}
		else {
			// Set colSpan 1 if not set otherwise all formatting hell breaks loose
			if (visitedItem.getColspan() == null) { // not set
				code.append("colSpan:1,");
			}
			// Set endRow false as well to stop formatting gayness
			// Since this is not an input widget, we can't use preProcessFormItem()
			// Take care of the title, as we're not calling preProcessFormItem
			String title = visitedItem.getLabel();
			if (title == null) {
				title = displayName;
			}
			title = SmartClientGenerateUtils.processString(Util.i18n(title, locale));
			code.append("endRow:false,title:'").append(SmartClientGenerateUtils.processString(title)).append("',type:'blurb',");
			if (binding == null) {
				code.append("defaultValue:'").append(SmartClientGenerateUtils.processString((value == null) ? Util.i18n(displayName, locale) : Util.i18n(value, locale), false, false));
			}
			else {
				code.append("name:'").append(BindUtil.sanitiseBinding(binding));
			}
			code.append("',");
			
			if (alignment != null) {
				code.append("textAlign:'").append(alignment.toAlignmentString()).append("',");
			}
			size(label, null, code);
			invisible(label.getInvisibleConditionName(), code);
		}
	}

	@Override
	public void visitParameter(Parameter parameter,
								boolean parentVisible,
								boolean parentEnabled) {
		// do nothing - parameters are handled separately
	}
	
	@Override
	public void visitFilterParameter(FilterParameter parameter,
										boolean parentVisible,
										boolean parentEnabled) {
		// do nothing - parameters are handled separately
	}

	// TODO implement
	@Override
	public void visitProgressBar(ProgressBar progressBar,
									boolean parentVisible,
									boolean parentEnabled) {
/*
		if (formID != null) {
			code.append("type:'canvas',showTitle:false,canvas:");

		}
*/
//TODO Make a value from CanvasItem.
		String variable = "v" + variableCounter++;
		code.append("var ").append(variable).append("=isc.BizLabel.create({value: '");
		code.append(progressBar.getBinding());
		code.append("'});\n");
		code.append(containerVariables.peek()).append(".addContained(").append(variable).append(");\n");
	}

	private String dataWidgetVariable = null;
	private Document dataWidgetDocument = null;
	private String dataWidgetBinding = null;
	// Indicates whether the field definition array has been completed and closed off
	// Its used to ensure the last ']' is appended before adding events or closing the grid definition
	private boolean dataWidgetFieldsIncomplete = false;
	
	@Override
	public void visitDataGrid(DataGrid grid,
								boolean parentVisible,
								boolean parentEnabled) {
		visitDataWidget(grid);
		if (Boolean.FALSE.equals(grid.getShowAdd())) {
			code.append("showAdd:false,");
		}
		if (Boolean.FALSE.equals(grid.getShowZoom())) {
			code.append("showZoom:false,");
		}
		if (Boolean.FALSE.equals(grid.getShowEdit())) {
			code.append("showEdit:false,");
		}
		if (Boolean.FALSE.equals(grid.getShowRemove())) {
			code.append("showRemove:false,");
		}
		if (Boolean.FALSE.equals(grid.getShowDeselect())) {
			code.append("showDeselect:false,");
		}
		if (Boolean.TRUE.equals(grid.getInline())) { // defaults to not being inline
			code.append("inline:true,");
		}
		if (Boolean.TRUE.equals(grid.getWordWrap())) { // defaults to not being wrapped
			code.append("wordWrap:true,");
		}
		disableCRUD(grid, code);
		String selectedIdBinding = grid.getSelectedIdBinding();
		if (selectedIdBinding != null) {
			code.append("selectedIdBinding:'").append(BindUtil.sanitiseBinding(selectedIdBinding)).append("',");
			TargetMetaData target = BindUtil.getMetaDataForBinding(customer, module, document, selectedIdBinding);
			code.append("selectedIdTrackChanges:").append(target.getAttribute().isTrackChanges()).append(',');
		}
		disabled(grid.getDisabledConditionName(), code);
		editable(grid.getEditable(), code);
		code.append("_fields:[");
	}
	
	@Override
	public void visitDataRepeater(DataRepeater repeater,
									boolean parentVisible,
									boolean parentEnabled) {
		visitDataWidget(repeater);
		code.append("isRepeater:true,");
		code.append("showColumnHeaders:").append(Boolean.TRUE.equals(repeater.getShowColumnHeaders()));
		code.append(",showGrid:").append(Boolean.TRUE.equals(repeater.getShowGrid()));
		code.append(",_fields:[");
	}
	
	private void visitDataWidget(AbstractDataWidget widget) {
		dataWidgetBinding = widget.getBinding();
		TargetMetaData target = BindUtil.getMetaDataForBinding(customer,
																module,
																document,
																dataWidgetBinding);
		Relation relation = (Relation) target.getAttribute();
		String documentName = relation.getDocumentName();

		dataWidgetDocument = module.getDocument(customer, documentName);
		dataWidgetVariable = "v" + variableCounter++;
		code.append("var ").append(dataWidgetVariable).append("=isc.BizDataGrid.create({_mod:'");
		code.append(dataWidgetDocument.getOwningModuleName());
		code.append("',_doc:'");
		code.append(dataWidgetDocument.getName());
		code.append("',_b:'").append(BindUtil.sanitiseBinding(dataWidgetBinding));
		code.append("',ID:").append(IDExpression());
		code.append(",canCreate:").append(user.canCreateDocument(dataWidgetDocument));
		code.append(",canUpdate:").append(user.canUpdateDocument(dataWidgetDocument));
		code.append(",canDelete:").append(user.canDeleteDocument(dataWidgetDocument)).append(',');
		String title = widget.getTitle();
		if (title != null) {
			code.append("title:'");
			code.append(SmartClientGenerateUtils.processString(Util.i18n(title, locale))).append("',");
		}
		if ((relation instanceof Collection) && 
				Boolean.TRUE.equals(((Collection) relation).getOrdered())) {
			code.append("_ordinal:'").append(Bean.ORDINAL_NAME).append("',");
		}
		size(widget, DEFAULT_MIN_HEIGHT_IN_PIXELS, code);
		invisible(widget.getInvisibleConditionName(), code);
		dataWidgetFieldsIncomplete = true;
		eventsWithNoForm = true;
	}

	@Override
	public void visitedDataGrid(DataGrid grid,
									boolean parentVisible,
									boolean parentEnabled) {
		visitedDataWidget();
	}

	@Override
	public void visitedDataRepeater(DataRepeater repeater,
										boolean parentVisible,
										boolean parentEnabled) {
		visitedDataWidget();
	}
	
	private void visitedDataWidget() {
		if (dataWidgetFieldsIncomplete) {
			code.setLength(code.length() - 1); // remove trailing comma from list grid field definition
			code.append("],");
		}
		code.append("_view:view});\n");
		code.append(containerVariables.peek()).append(".addContained(").append(dataWidgetVariable).append(");\n");
		dataWidgetVariable = null;
		dataWidgetDocument = null;
		dataWidgetBinding = null;
		dataWidgetFieldsIncomplete = false;
		eventsWithNoForm = false;
	}
	
	private InputWidget dataWidgetColumnInputWidget;
	
	@Override
	public void visitDataGridBoundColumn(DataGridBoundColumn column,
											boolean parentVisible,
											boolean parentEnabled) {
		// do nothing
	}

	@Override
	public void visitedDataGridBoundColumn(DataGridBoundColumn column,
											boolean parentVisible,
											boolean parentEnabled) {
		if (dataWidgetColumnInputWidget != null) {
			SmartClientDataGridFieldDefinition def = null;
			String binding = column.getBinding();
			if (binding == null) { // column bound to collection for the grid
				def = SmartClientGenerateUtils.getDataGridField(user,
																	customer,
																	module, 
																	document, 
																	dataWidgetColumnInputWidget, 
																	dataWidgetBinding);
			} 
			else {
				def = SmartClientGenerateUtils.getDataGridField(user,
																	customer,
																	module, 
																	dataWidgetDocument, 
																	dataWidgetColumnInputWidget, 
																	null);
			}

			String title = column.getTitle();
			if (title != null) {
				def.setTitle(Util.i18n(title, locale));
			}
			HorizontalAlignment textAlignment = column.getAlignment();
			if (textAlignment != null) {
				def.setAlign(textAlignment);
			}
			
			def.setEditable(! Boolean.FALSE.equals(column.getEditable()));
			def.setPixelWidth(column.getPixelWidth());
			code.append('{').append(def.toJavascript()).append("},");

			SmartClientLookupDefinition lookup = def.getLookup();
			if (lookup != null) {
				StringBuilder ds = new StringBuilder(64);
				String optionDataSource = lookup.getOptionDataSource();
				SmartClientGenerateUtils.appendDataSourceDefinition(user,
																		customer, 
																		lookup.getQuery(),
																		optionDataSource,
																		(Lookup) dataWidgetColumnInputWidget, 
																		false,
																		ds,
																		null);
				code.insert(0, ds);
			}
			dataWidgetColumnInputWidget = null;
		}
	}

	@Override
	public void visitDataGridContainerColumn(DataGridContainerColumn column,
												boolean parentVisible,
												boolean parentEnabled) {
		code.append("{name:'_").append(formatCounter++);
		code.append("',type:'text',formatCellValue:'value;',canEdit:false,title:'");
		
		String title = column.getTitle();
		code.append((title == null) ? " " : SmartClientGenerateUtils.processString(Util.i18n(title, locale))).append('\'');
		HorizontalAlignment alignment = column.getAlignment();
		if (alignment != null) {
			code.append(",align:'").append(alignment.toAlignmentString()).append('\'');
		}
		Integer width = column.getPixelWidth();
		if (width != null) {
			code.append(",width:").append(width);
		}
		code.append("},");
	}

	@Override
	public void visitedDataGridContainerColumn(DataGridContainerColumn column,
												boolean parentVisible,
												boolean parentEnabled) {
		// do nothing
	}
	
	private String listWidgetVariable = null;

	@Override
	public void visitListGrid(ListGrid grid,
								boolean parentVisible,
								boolean parentEnabled) {
		visitListWidget(grid);
		visitGrid(grid);
	}

	@Override
	public void visitListRepeater(ListRepeater repeater, boolean parentVisible, boolean parentEnabled) {
		visitListWidget(repeater);
		code.append("isRepeater:true,");
		code.append("showColumnHeaders:").append(Boolean.TRUE.equals(repeater.getShowColumnHeaders())).append(',');
		code.append("showGrid:").append(Boolean.TRUE.equals(repeater.getShowGrid())).append(',');
	}
	
	@Override
	public void visitTreeGrid(TreeGrid grid,
								boolean parentVisible,
								boolean parentEnabled) {
		visitListWidget(grid);
		visitGrid(grid);
		String rootBinding = grid.getRootIdBinding();
		if (rootBinding != null) {
			code.append("rootIdBinding:'").append(BindUtil.sanitiseBinding(rootBinding)).append("',");
		}
		code.append("isTree:true,");
	}

	private void visitListWidget(AbstractListWidget widget) {
		String queryName = widget.getQueryName();
		String modelName = widget.getModelName();
		String dataSourceId = null;
		if (queryName != null) { // its a query
			DocumentQueryDefinition query = module.getDocumentQuery(queryName);
			StringBuilder ds = new StringBuilder(256);
			dataSourceId = SmartClientGenerateUtils.appendDataSourceDefinition(user, customer, query, null, null, false, ds, null);
			code.insert(0, ds);
		}
		else {
			if (modelName != null) { // its a model
				StringBuilder ds = new StringBuilder(256);
				dataSourceId = SmartClientGenerateUtils.appendDataSourceDefinition(user, 
																					customer, 
																					module, 
																					document,
																					modelName,
																					false,
																					ds, 
																					null);
				code.insert(0, ds);
			}
			else {
				DocumentQueryDefinition query = module.getDocumentDefaultQuery(customer, document.getName());
				StringBuilder ds = new StringBuilder(256);
				dataSourceId = SmartClientGenerateUtils.appendDataSourceDefinition(user, customer, query, null, null, false, ds, null);
				code.insert(0, ds);
			}
		}
		
		listWidgetVariable = "v" + variableCounter++;
		code.append("var ").append(listWidgetVariable).append("=isc.BizListGrid.create({");
		code.append("ID:").append(IDExpression()).append(',');
		code.append("dataSource:'").append(dataSourceId).append("',");
		code.append("name:'").append(listWidgetVariable).append("',");
		String title = widget.getTitle();
		if (title != null) {
			code.append("title:'");
			code.append(SmartClientGenerateUtils.processString(Util.i18n(title, locale))).append("',");
		}
		String postRefreshConditionName = widget.getPostRefreshConditionName();
		if (postRefreshConditionName != null) {
			code.append("postRefreshConditionName:'").append(postRefreshConditionName).append("',");
		}
		size(widget, DEFAULT_MIN_HEIGHT_IN_PIXELS, code);
		invisible(widget.getInvisibleConditionName(), code);
		
		eventsWithNoForm = true;
	}
	
	private void visitGrid(ListGrid grid) {
		code.append("contConv:").append(grid.getContinueConversation()).append(",");
		String selectedIdBinding = grid.getSelectedIdBinding();
		if (selectedIdBinding != null) {
			code.append("selectedIdBinding:'").append(BindUtil.sanitiseBinding(selectedIdBinding)).append("',");
			TargetMetaData target = BindUtil.getMetaDataForBinding(customer, module, document, selectedIdBinding);
			code.append("selectedIdTrackChanges:").append(target.getAttribute().isTrackChanges()).append(',');
		}
		disabled(grid.getDisabledConditionName(), code);
		disableCRUD(grid, code);
		if (Boolean.FALSE.equals(grid.getShowAdd())) {
			code.append("showAdd:false,");
		}
		if (Boolean.FALSE.equals(grid.getShowZoom())) {
			code.append("showZoom:false,");
		}
		if (Boolean.FALSE.equals(grid.getShowEdit())) {
			code.append("showEdit:false,");
		}
		if (Boolean.FALSE.equals(grid.getShowRemove())) {
			code.append("showRemove:false,");
		}
		if (Boolean.FALSE.equals(grid.getShowDeselect())) {
			code.append("showDeselect:false,");
		}
		if (Boolean.FALSE.equals(grid.getShowFilter())) {
			code.append("showFilter:false,");
		}
		if (Boolean.FALSE.equals(grid.getShowSummary())) {
			code.append("showSummary:false,");
		}
		if (Boolean.FALSE.equals(grid.getShowExport())) {
			code.append("showExport:false,");
		}
		if (Boolean.FALSE.equals(grid.getShowSnap())) {
			code.append("showSnap:false,");
		}
		if (Boolean.FALSE.equals(grid.getShowTag())) {
			code.append("showTag:false,");
		}
		if (Boolean.FALSE.equals(grid.getAutoPopulate())) {
			code.append("autoPopulate:false,");
		}
	}

	@Override
	public void visitedListGrid(ListGrid grid,
									boolean parentVisible,
									boolean parentEnabled) {
		appendFilterParameters(grid.getParameters(), code);
		visitedListWidget();
	}

	@Override
	public void visitedListRepeater(ListRepeater repeater, 
										boolean parentVisible,
										boolean parentEnabled) {
		visitedListWidget();
	}
	
	@Override
	public void visitedTreeGrid(TreeGrid grid,
									boolean parentVisible,
									boolean parentEnabled) {
		appendFilterParameters(grid.getParameters(), code);
		visitedListWidget();
	}

	private void visitedListWidget() {
		code.append("_view:view});\n");
		code.append(containerVariables.peek()).append(".addContained(").append(listWidgetVariable).append(");\n");
		listWidgetVariable = null;
		eventsWithNoForm = false;
	}

	@Override
	public void visitCheckBox(CheckBox checkBox,
								boolean parentVisible,
								boolean parentEnabled) {
		if (dataWidgetVariable != null) {
			dataWidgetColumnInputWidget = checkBox;
			return;
		}
		
		preProcessFormItem(checkBox, "checkbox");
		size(checkBox, null, code);
		if (! Boolean.FALSE.equals(checkBox.getTriState())) {
			code.append("allowEmptyValue:true,");
		}
		code.append("labelAsTitle:true,");
		disabled(checkBox.getDisabledConditionName(), code);
		invisible(checkBox.getInvisibleConditionName(), code);
	}

	@Override
	public void visitedCheckBox(CheckBox checkBox,
									boolean parentVisible,
									boolean parentEnabled) {
		// do nothing
	}

	// TODO implement this - does this need size? probably
	@Override
	public void visitCheckMembership(CheckMembership membership,
										boolean parentVisible,
										boolean parentEnabled) {
		String variable = "v" + variableCounter++;
		code.append("var ").append(variable).append("=isc.BizLabel.create({value:'");
		code.append("check membership").append(membership.getBinding());
		code.append("'});\n");
		code.append(containerVariables.peek()).append(".addContained(").append(variable).append(");\n");
	}

	@Override
	public void visitedCheckMembership(CheckMembership membership,
										boolean parentVisible, 
										boolean parentEnabled) {
		// do nothing - until implemented properly
	}

	@Override
	public void visitColourPicker(ColourPicker colour,
									boolean parentVisible,
									boolean parentEnabled) {
		if (dataWidgetVariable != null) {
			dataWidgetColumnInputWidget = colour;
			return;
		}

		if (formVariable == null) {
			throw new MetaDataException("Colour found without a form");
		}

		preProcessFormItem(colour, "color");
		size(colour, null, code);
		disabled(colour.getDisabledConditionName(), code);
		invisible(colour.getInvisibleConditionName(), code);
	}

	@Override
	public void visitedColourPicker(ColourPicker colour,
										boolean parentVisible,
										boolean parentEnabled) {
		// do nothing
	}

	@Override
	public void visitCombo(Combo combo,
							boolean parentVisible,
							boolean parentEnabled) {
		if (dataWidgetVariable != null) {
			dataWidgetColumnInputWidget = combo;
			return;
		}

		if (formVariable == null) {
			throw new MetaDataException("Combo found without a form");
		}

		preProcessFormItem(combo, "select");
		size(combo, null, code);
		disabled(combo.getDisabledConditionName(), code);
		invisible(combo.getInvisibleConditionName(), code);
	}

	@Override
	public void visitedCombo(Combo combo,
								boolean parentVisible,
								boolean parentEnabled) {
		// do nothing
	}

	@Override
	public void visitContentImage(ContentImage image,
									boolean parentVisible, 
									boolean parentEnabled) {
		// markup is generated in the JSON data for a data grid container column content image
		if (dataWidgetVariable != null) {
			dataWidgetColumnInputWidget = image;
			return;
		}

		if (formVariable == null) {
			throw new MetaDataException("ContentImage found without a form");
		}

		preProcessFormItem(image, "bizContentImage");
		size(image, null, code);
		disabled(image.getDisabledConditionName(), code);
		invisible(image.getInvisibleConditionName(), code);
		editable(image.getEditable(), code);
	}

	@Override
	public void visitContentLink(ContentLink link,
									boolean parentVisible,
									boolean parentEnabled) {
		if (dataWidgetVariable != null) {
			dataWidgetColumnInputWidget = link;
			return;
		}

		if (formVariable == null) {
			throw new MetaDataException("ContentLink found without a form");
		}

		preProcessFormItem(link, "bizContentLink");
		String value = link.getValue();
		if (value != null) {
			code.append("value:'").append(SmartClientGenerateUtils.processString(Util.i18n(value, locale))).append("',");
		}
		disabled(link.getDisabledConditionName(), code);
		invisible(link.getInvisibleConditionName(), code);
		editable(link.getEditable(), code);
	}

	@Override
	public void visitRichText(RichText text,
								boolean parentVisible,
								boolean parentEnabled) {
		if (dataWidgetVariable != null) {
			dataWidgetColumnInputWidget = text;
			return;
		}

		if (formVariable == null) {
			throw new MetaDataException("RichText found without a form");
		}

		preProcessFormItem(text, "richText");
		size(text, DEFAULT_MIN_HEIGHT_IN_PIXELS, code);
		disabled(text.getDisabledConditionName(), code);
		invisible(text.getInvisibleConditionName(), code);
	}

	@Override
	public void visitedRichText(RichText richText,
									boolean parentVisible,
									boolean parentEnabled) {
		// do nothing
	}

	@Override
	public void visitHTML(HTML html,
							boolean parentVisible,
							boolean parentEnabled) {
		if (dataWidgetVariable != null) {
			dataWidgetColumnInputWidget = html;
			return;
		}

		if (formVariable == null) {
			throw new MetaDataException("HTML found without a form");
		}

		preProcessFormItem(html, "bizHTML");
		size(html, null, code);
		disabled(html.getDisabledConditionName(), code);
		invisible(html.getInvisibleConditionName(), code);
	}

	// indicates if we are visiting a list membership, list grid or data grid widget.
	// this allow specific javascript for events to be generated since these widgets live outside of a form
	private boolean eventsWithNoForm = false;
	
	@Override
	public void visitListMembership(ListMembership membership,
										boolean parentVisible,
										boolean parentEnabled) {
		eventsWithNoForm = true;
		
		String membershipBinding = membership.getBinding();
		TargetMetaData target = BindUtil.getMetaDataForBinding(customer,
																module,
																document,
																membershipBinding);
		Relation relation = (Relation) target.getAttribute();

		String variable = "v" + variableCounter++;
		code.append("var ").append(variable).append("=isc.BizListMembership.create({_b:'");
		code.append(BindUtil.sanitiseBinding(membershipBinding));
		code.append('\'');
		String heading = membership.getCandidatesHeading();
		if (heading != null) {
			code.append(",candidatesHeading:'");
			code.append(SmartClientGenerateUtils.processString(Util.i18n(heading, locale))).append('\'');
		}
		heading = membership.getMembersHeading();
		if (heading != null) {
			code.append(",membersHeading:'");
			code.append(SmartClientGenerateUtils.processString(Util.i18n(heading, locale))).append('\'');
		}
		if ((relation instanceof Collection) && 
				Boolean.TRUE.equals(((Collection) relation).getOrdered())) {
			code.append(",_ordinal:'").append(Bean.ORDINAL_NAME).append('\'');
		}
		code.append(",_view:view,");
		disabled(membership.getDisabledConditionName(), code);
		invisible(membership.getInvisibleConditionName(), code);
		
		containerVariables.push(variable);
	}
	
	@Override
	public void visitedListMembership(ListMembership membership,
										boolean parentVisible,
										boolean parentEnabled) {
		removeTrailingComma(code);
		code.append("});\n");

		String variable = containerVariables.pop();
		code.append(containerVariables.peek()).append(".addContained(").append(variable).append(");\n");

		eventsWithNoForm = false;
	}

	@Override
	public void visitComparison(Comparison comparison,
									boolean parentVisible,
									boolean parentEnabled) {
		String variable = "v" + variableCounter++;
		code.append("var ").append(variable).append("=isc.BizComparison.create({_b:'");
		code.append(BindUtil.sanitiseBinding(comparison.getBinding()));
		code.append("',_view:view,");
		editable(comparison.getEditable(), code);
		disabled(comparison.getDisabledConditionName(), code);
		invisible(comparison.getInvisibleConditionName(), code);
		size(comparison, DEFAULT_MIN_HEIGHT_IN_PIXELS, code);
		removeTrailingComma(code);
		code.append("});\n");
		code.append(containerVariables.peek()).append(".addContained(").append(variable).append(");\n");
	}

	@Override
	public void visitLookupDescription(LookupDescription lookup,
										boolean parentVisible,
										boolean parentEnabled) {
		if (dataWidgetVariable != null) {
			dataWidgetColumnInputWidget = lookup;
			return;
		}

		if (formVariable == null) {
			throw new MetaDataException("LookupDescription found without a form");
		}

		SmartClientFieldDefinition def = preProcessFormItem(lookup, "bizLookupDescription");
		size(lookup, null, code);
		disabled(lookup.getDisabledConditionName(), code);
		invisible(lookup.getInvisibleConditionName(), code);
		editable(lookup.getEditable(), code);
		disableLookupComponents(lookup, code);
    	code.append("canCreate:").append(def.getLookup().getCanCreate());
    	code.append(",canUpdate:").append(def.getLookup().getCanUpdate());

		code.append(",_view:view,");
		appendFilterParameters(lookup.getParameters(), code);

		DocumentQueryDefinition query = def.getLookup().getQuery();

		StringBuilder ds = new StringBuilder(256);
		String optionDataSource = def.getLookup().getOptionDataSource();
		SmartClientGenerateUtils.appendDataSourceDefinition(user,
																customer,
																query,
																optionDataSource,
																lookup,
																false,
																ds,
																null);
		code.insert(0, ds);
	}

	@Override
	public void visitedLookupDescription(LookupDescription lookup,
											boolean parentVisible,
											boolean parentEnabled) {
		// do nothing
	}

	@Override
	public void visitLookup(Lookup lookup,
								boolean parentVisible,
								boolean parentEnabled) {
		if (dataWidgetVariable != null) {
			dataWidgetColumnInputWidget = lookup;
			return;
		}

		if (formVariable == null) {
			throw new MetaDataException("Lookup found without a form");
		}

		code.append("type:'blurb',defaultValue:'lookup ");
		code.append(lookup.getBinding()).append("',");
		disableLookupComponents(lookup, code);
		appendFilterParameters(lookup.getParameters(), code);
	}

	@Override
	public void visitedLookup(Lookup lookup,
								boolean parentVisible,
								boolean parentEnabled) {
		// do nothing
	}

	@Override
	public void visitPassword(Password password, 
								boolean parentVisible,
								boolean parentEnabled) {
		if (dataWidgetVariable != null) {
			dataWidgetColumnInputWidget = password;
			return;
		}

		if (formVariable == null) {
			throw new MetaDataException("Password found without a form");
		}

		preProcessFormItem(password, "password");
		size(password, null, code);
		disabled(password.getDisabledConditionName(), code);
		invisible(password.getInvisibleConditionName(), code);
	}

	@Override
	public void visitedPassword(Password password,
									boolean parentVisible,
									boolean parentEnabled) {
		// do nothing
	}

	@Override
	public void visitRadio(Radio radio,
							boolean parentVisible,
							boolean parentEnabled) {
		if (dataWidgetVariable != null) {
			dataWidgetColumnInputWidget = radio;
			return;
		}

		if (formVariable == null) {
			throw new MetaDataException("Radio found without a form");
		}

		preProcessFormItem(radio, "radioGroup");
		size(radio, null, code);
		if (Boolean.FALSE.equals(radio.getVertical())) {
			code.append("vertical:false,");
		}
		disabled(radio.getDisabledConditionName(), code);
		invisible(radio.getInvisibleConditionName(), code);
	}

	@Override
	public void visitedRadio(Radio radio,
								boolean parentVisible,
								boolean parentEnabled) {
		// do nothing
	}

	@Override
	public void visitSlider(Slider slider, 
								boolean parentVisible,
								boolean parentEnabled) {
		if (dataWidgetVariable != null) {
			dataWidgetColumnInputWidget = slider;
			return;
		}

		if (formVariable == null) {
			throw new MetaDataException("Slider found without a form");
		}
		
		preProcessFormItem(slider, "slider");
		Double min = slider.getMin();
		if (min != null) {
			code.append("minValue:").append(min).append(',');
		}
		Double max = slider.getMax();
		if (max != null) {
			code.append("maxValue:").append(max).append(',');
		}
		Integer numberOfDiscreteValues = slider.getNumberOfDiscreteValues();
		if (numberOfDiscreteValues != null) {
			code.append("numValues:").append(numberOfDiscreteValues).append(',');
		}
		Integer roundingPrecision = slider.getRoundingPrecision();
		if ((roundingPrecision != null) && (roundingPrecision.intValue() != 0)) {
			code.append("roundValues:false,roundingPrecision:").append(roundingPrecision).append(',');
		}
		if (Boolean.TRUE.equals(slider.getVertical())) {
			code.append("vertical:true,");
		}
		size(slider, null, code);
		disabled(slider.getDisabledConditionName(), code);
		invisible(slider.getInvisibleConditionName(), code);
	}

	@Override
	public void visitedSlider(Slider slider,
								boolean parentVisible,
								boolean parentEnabled) {
		// do nothing
	}

	@Override
	public void visitSpinner(Spinner spinner, 
								boolean parentVisible,
								boolean parentEnabled) {
		if (dataWidgetVariable != null) {
			dataWidgetColumnInputWidget = spinner;
			return;
		}

		if (formVariable == null) {
			throw new MetaDataException("Spinner found without a form");
		}
		
		preProcessFormItem(spinner, "spinner");
		Double min = spinner.getMin();
		if (min != null) {
			code.append("min:").append(min).append(',');
		}
		Double max = spinner.getMax();
		if (max != null) {
			code.append("max:").append(max).append(',');
		}
		Double step = spinner.getStep();
		if (step != null) {
			code.append("step:").append(step).append(',');
		}
		size(spinner, null, code);
		disabled(spinner.getDisabledConditionName(), code);
		invisible(spinner.getInvisibleConditionName(), code);
	}

	@Override
	public void visitedSpinner(Spinner spinner,
								boolean parentVisible,
								boolean parentEnabled) {
		// do nothing
	}

	@Override
	public void visitTextArea(TextArea text, 
								boolean parentVisible,
								boolean parentEnabled) {
		if (dataWidgetVariable != null) {
			dataWidgetColumnInputWidget = text;
			return;
		}

		if (formVariable == null) {
			throw new MetaDataException("TextArea found without a form");
		}

		preProcessFormItem(text, "textArea");
		size(text, null, code);
		disabled(text.getDisabledConditionName(), code);
		invisible(text.getInvisibleConditionName(), code);

		if (Boolean.FALSE.equals(text.getEditable())) {
			code.append("canEdit:false,");
		}

		// Highlight text on focus
		code.append("selectOnFocus:true,");
	}

	@Override
	public void visitedTextArea(TextArea text,
									boolean parentVisible,
									boolean parentEnabled) {
		// do nothing
	}

	@Override
	public void visitTextField(TextField text,
								boolean parentVisible,
								boolean parentEnabled) {
		if (dataWidgetVariable != null) {
			dataWidgetColumnInputWidget = text;
			return;
		}

		if (formVariable == null) {
			throw new MetaDataException("TextField found without a form");
		}

		if (Boolean.TRUE.equals(text.getPreviousValues())) {
		    preProcessFormItem(text, "comboBox");
            TargetMetaData target = BindUtil.getMetaDataForBinding(customer, module, document, text.getBinding());
            Document targetDocument = target.getDocument();
            // have the options of 
			// 1) pickListCriteria:{}
			// 2) optionCriteria:{}
			// 3) optionFilterContext:{params{}}
			// 4) getPickListFilterCriteria: function() {}
			code.append("optionDataSource:isc.BizUtil.PREVIOUS_VALUES_DATA_SOURCE,");
			code.append("optionFilterContext:{params:{").append(AbstractWebContext.MODULE_NAME).append(":'");
			code.append(targetDocument.getOwningModuleName());
			code.append("',").append(AbstractWebContext.DOCUMENT_NAME).append(":'").append(targetDocument.getName());
			code.append("',").append(AbstractWebContext.BINDING_NAME).append(":'").append(target.getAttribute().getName());
			// Use the home-grown previous values style override in basec.css and don't show the picker icon
			code.append("'}},textBoxStyle:'bizhubPreviousValuesText',showPickerIcon:false,");
			// Set the dropdown selection field mapping
			code.append("valueField:'value',");
			code.append("displayField:'value',");
			code.append("fetchMissingValues:false,");
			code.append("selectOnFocus:true,completeOnTab:true,");
			
//do I need these ones also?
/*
pickListFields:[{name:'value'}],
*/
		}
		else {
			preProcessFormItem(text, null);
		}

		size(text, null, code);
		disabled(text.getDisabledConditionName(), code);
		invisible(text.getInvisibleConditionName(), code);
		
		if (Boolean.FALSE.equals(text.getEditable())) {
			code.append("canEdit:false,");
		}
		
		// Highlight text on focus
		code.append("selectOnFocus:true,");
	}

	@Override
	public void visitedTextField(TextField text,
									boolean parentVisible,
									boolean parentEnabled) {
		// do nothing
	}

	@Override
	public void visitInject(Inject inject, boolean parentVisible, boolean parentEnabled) {
		if (visitedItem != null) {
			// NB instead of preprocessFormItem(), handle title and required
			String value = visitedItem.getLabel();
			if (value != null) {
				code.append("title:'").append(UtilImpl.processStringValue(value)).append("',");
			}
			Boolean required = visitedItem.getRequired();
			if (Boolean.TRUE.equals(required)) {
				code.append("required:true,");
			}
		}
		code.append(inject.getScript());
	}

	@Override
	public void visitedView() {
		containerVariables.pop();
		if (! noCreateView) {
			if (ViewType.edit.toString().equals(view.getName())) {
				code.append("view.addContained(edit);");
			}
			else if (ViewType.create.toString().equals(view.getName())) {
				code.append("view.addContained(create);");
			}
		}
		if (! viewHasAtLeastOneForm) {
			String var = "v" + variableCounter++;
			code.append("var ").append(var).append("=isc.DynamicForm.create({invisibleConditionName:'true'});");
			code.append("view._vm.addMember(").append(var).append(");");
			code.append("view.addContained(").append(var).append(");\n");
		}
	}

	@Override
	public void visitedVBox(VBox vbox,
								boolean parentVisible,
								boolean parentEnabled) {
		containerVariables.pop();
	}

	@Override
	public void visitedHBox(HBox hbox,
								boolean parentVisible,
								boolean parentEnabled) {
		containerVariables.pop();
	}

	@Override
	public void visitCustomAction(ActionImpl action) {
		addAction(action.getResourceName(), 
					null, 
					action.getDisplayName(),
					action.getInActionPanel(),
					action.getClientValidation(),
					action.getRelativeIconFileName(),
					action.getIconStyleClass(),
					action.getToolTip(),
					action.getConfirmationText(),
					action.getParameters(),
					action.getDisabledConditionName(),
					action.getInvisibleConditionName());
	}

	@Override
	public void visitAddAction(ActionImpl action) {
		addAction(null,
					ImplicitActionName.Add,
					action.getDisplayName(),
					action.getInActionPanel(),
					action.getClientValidation(),
					action.getRelativeIconFileName(),
					action.getIconStyleClass(),
					action.getToolTip(),
					action.getConfirmationText(),
					action.getParameters(),
					action.getDisabledConditionName(),
					action.getInvisibleConditionName());
	}

	@Override
	public void visitRemoveAction(ActionImpl action) {
		addAction(null,
					ImplicitActionName.Remove,
					action.getDisplayName(),
					action.getInActionPanel(),
					action.getClientValidation(),
					action.getRelativeIconFileName(),
					action.getIconStyleClass(),
					action.getToolTip(),
					action.getConfirmationText(),
					action.getParameters(),
					action.getDisabledConditionName(),
					action.getInvisibleConditionName());
	}

	@Override
	public void visitZoomOutAction(ActionImpl action) {
		addAction(null,
					ImplicitActionName.ZoomOut,
					action.getDisplayName(),
					action.getInActionPanel(),
					action.getClientValidation(),
					action.getRelativeIconFileName(),
					action.getIconStyleClass(),
					action.getToolTip(),
					action.getConfirmationText(),
					action.getParameters(),
					action.getDisabledConditionName(),
					action.getInvisibleConditionName());
	}

	@Override
	public void visitNavigateAction(ActionImpl action) {
		addAction(null,
					ImplicitActionName.Navigate,
					action.getDisplayName(),
					action.getInActionPanel(),
					action.getClientValidation(),
					action.getRelativeIconFileName(),
					action.getIconStyleClass(),
					action.getToolTip(),
					action.getConfirmationText(),
					action.getParameters(),
					action.getDisabledConditionName(),
					action.getInvisibleConditionName());
	}

	@Override
	public void visitOKAction(ActionImpl action) {
		addAction(null,
					ImplicitActionName.OK,
					action.getDisplayName(),
					action.getInActionPanel(),
					action.getClientValidation(),
					action.getRelativeIconFileName(),
					action.getIconStyleClass(),
					action.getToolTip(),
					action.getConfirmationText(),
					action.getParameters(),
					action.getDisabledConditionName(),
					action.getInvisibleConditionName());
	}

	@Override
	public void visitSaveAction(ActionImpl action) {
		addAction(null,
					ImplicitActionName.Save,
					action.getDisplayName(),
					action.getInActionPanel(),
					action.getClientValidation(),
					action.getRelativeIconFileName(),
					action.getIconStyleClass(),
					action.getToolTip(),
					action.getConfirmationText(),
					action.getParameters(),
					action.getDisabledConditionName(),
					action.getInvisibleConditionName());
	}

	@Override
	public void visitCancelAction(ActionImpl action) {
		addAction(null,
					ImplicitActionName.Cancel,
					action.getDisplayName(),
					action.getInActionPanel(),
					action.getClientValidation(),
					action.getRelativeIconFileName(),
					action.getIconStyleClass(),
					action.getToolTip(),
					action.getConfirmationText(),
					action.getParameters(),
					action.getDisabledConditionName(),
					action.getInvisibleConditionName());
	}

	@Override
	public void visitDeleteAction(ActionImpl action) {
		addAction(null,
					ImplicitActionName.Delete,
					action.getDisplayName(),
					action.getInActionPanel(),
					action.getClientValidation(),
					action.getRelativeIconFileName(),
					action.getIconStyleClass(),
					action.getToolTip(),
					action.getConfirmationText(),
					action.getParameters(),
					action.getDisabledConditionName(),
					action.getInvisibleConditionName());
	}

	@Override
	public void visitReportAction(ActionImpl action) {
		addAction(null,
					ImplicitActionName.Report,
					action.getDisplayName(),
					action.getInActionPanel(),
					action.getClientValidation(),
					action.getRelativeIconFileName(),
					action.getIconStyleClass(),
					action.getToolTip(),
					action.getConfirmationText(),
					action.getParameters(),
					action.getDisabledConditionName(),
					action.getInvisibleConditionName());
	}

	@Override
	public void visitBizExportAction(ActionImpl action) {
		addAction(action.getResourceName(),
					ImplicitActionName.BizExport,
					action.getDisplayName(),
					action.getInActionPanel(),
					action.getClientValidation(),
					action.getRelativeIconFileName(),
					action.getIconStyleClass(),
					action.getToolTip(),
					action.getConfirmationText(),
					action.getParameters(),
					action.getDisabledConditionName(),
					action.getInvisibleConditionName());
	}

	@Override
	public void visitBizImportAction(ActionImpl action) {
		addAction(action.getResourceName(),
					ImplicitActionName.BizImport,
					action.getDisplayName(),
					action.getInActionPanel(),
					action.getClientValidation(),
					action.getRelativeIconFileName(),
					action.getIconStyleClass(),
					action.getToolTip(),
					action.getConfirmationText(),
					action.getParameters(),
					action.getDisabledConditionName(),
					action.getInvisibleConditionName());
	}

	@Override
	public void visitDownloadAction(ActionImpl action) {
		addAction(action.getResourceName(),
					ImplicitActionName.Download,
					action.getDisplayName(),
					action.getInActionPanel(),
					action.getClientValidation(),
					action.getRelativeIconFileName(),
					action.getIconStyleClass(),
					action.getToolTip(),
					action.getConfirmationText(),
					action.getParameters(),
					action.getDisabledConditionName(),
					action.getInvisibleConditionName());
	}

	@Override
	public void visitUploadAction(ActionImpl action) {
		addAction(action.getResourceName(),
					ImplicitActionName.Upload,
					action.getDisplayName(),
					action.getInActionPanel(),
					action.getClientValidation(),
					action.getRelativeIconFileName(),
					action.getIconStyleClass(),
					action.getToolTip(),
					action.getConfirmationText(),
					action.getParameters(),
					action.getDisabledConditionName(),
					action.getInvisibleConditionName());
	}

	@Override
	public void visitNewAction(ActionImpl action) {
		addAction(null,
					ImplicitActionName.New,
					action.getDisplayName(),
					action.getInActionPanel(),
					action.getClientValidation(),
					action.getRelativeIconFileName(),
					action.getIconStyleClass(),
					action.getToolTip(),
					action.getConfirmationText(),
					action.getParameters(),
					action.getDisabledConditionName(),
					action.getInvisibleConditionName());
	}

	@Override
	public void visitEditAction(ActionImpl action) {
		addAction(null,
					ImplicitActionName.Edit,
					action.getDisplayName(),
					action.getInActionPanel(),
					action.getClientValidation(),
					action.getRelativeIconFileName(),
					action.getIconStyleClass(),
					action.getToolTip(),
					action.getConfirmationText(),
					action.getParameters(),
					action.getDisabledConditionName(),
					action.getInvisibleConditionName());
	}

	@Override
	public void visitPrintAction(ActionImpl action) {
		addAction(null,
				ImplicitActionName.Print,
				action.getDisplayName(),
				action.getInActionPanel(),
				action.getClientValidation(),
				action.getRelativeIconFileName(),
				action.getIconStyleClass(),
				action.getToolTip(),
				action.getConfirmationText(),
				action.getParameters(),
				action.getDisabledConditionName(),
				action.getInvisibleConditionName());
	}

	@Override
	public void visitOnChangedEventHandler(Changeable changeable,
											boolean parentVisible,
											boolean parentEnabled) {
		if (eventsWithNoForm) {
			code.append("changed:function(){var view=this._view;");
		}
		else {
			code.append("changed:function(form,item,value){var view=form._view;");
		}
	}

	@Override
	public void visitedOnChangedEventHandler(Changeable changeable,
												boolean parentVisible,
												boolean parentEnabled) {
		code.append("},");
	}

	@Override
	public void visitOnFocusEventHandler(Focusable blurable,
											boolean parentVisible,
											boolean parentEnabled) {
		// Note the test to short circuit focus event processing whilst requests are pending to stop loops with multiple fields.
		code.append("editorEnter:function(form,item,value){if((!isc.RPCManager.requestsArePending())&&item.validate()){var view=form._view;");
	}

	@Override
	public void visitedOnFocusEventHandler(Focusable blurable,
											boolean parentVisible,
											boolean parentEnabled) {
		code.append("}},");
	}

	// indicates that we are blurring and we need to call special methods
	// to potentially serialize calls to button actions after editorExit.
	private boolean visitingOnBlur = false;
	
	@Override
	public void visitOnBlurEventHandler(Focusable blurable,
											boolean parentVisible,
											boolean parentEnabled) {
		visitingOnBlur = true;
		
		// This fires before the BizButton action() method if a button was clicked
		// Note the test to short circuit blur event processing whilst requests are pending to stop loops with multiple fields.
		code.append("blur:function(form,item){if(!isc.RPCManager.requestsArePending()){form._view._blurry=item;}},");
		// This is called before or after the BizButton action depending on the browser.
		// Note the test to short circuit blur event processing whilst requests are pending to stop loops with multiple fields.
		code.append("editorExit:function(form,item,value){if((!isc.RPCManager.requestsArePending())&&item.validate()){var view=form._view;");
	}

	@Override
	public void visitedOnBlurEventHandler(Focusable blurable,
											boolean parentVisible,
											boolean parentEnabled) {
		code.append("}},");
		visitingOnBlur = false;
	}

	// Used to sort out server-side events into the bizEditedForServer() method.
	private boolean inOnAddedEventHandler = false;

	@Override
	public void visitOnAddedEventHandler(Addable addable,
											boolean parentVisible,
											boolean parentEnabled) {
		if (dataWidgetFieldsIncomplete) {
			code.setLength(code.length() - 1);
			code.append("],");
			dataWidgetFieldsIncomplete = false;
		}
		inOnAddedEventHandler = true;
		if (eventsWithNoForm) {
			code.append("bizAdded:function(){var view=this._view;");
		}
		else {
			code.append("bizAdded:function(form,item,value){var view=form._view;");
		}
	}

	@Override
	public void visitedOnAddedEventHandler(Addable addable,
											boolean parentVisible,
											boolean parentEnabled) {
		code.append("},");
		inOnAddedEventHandler = false;
	}

	// Used to sort out server-side events into the bizEditedForServer() method.
	private boolean inOnEditedEventHandler = false;

	@Override
	public void visitOnEditedEventHandler(Editable editable,
											boolean parentVisible,
											boolean parentEnabled) {
		if (dataWidgetFieldsIncomplete) {
			code.setLength(code.length() - 1);
			code.append("],");
			dataWidgetFieldsIncomplete = false;
		}
		inOnEditedEventHandler = true;
		if (eventsWithNoForm) {
			code.append("bizEdited:function(){var view=this._view;");
		}
		else {
			code.append("bizEdited:function(form,item,value){var view=form._view;");
		}
	}

	@Override
	public void visitedOnEditedEventHandler(Editable editable,
												boolean parentVisible,
												boolean parentEnabled) {
		code.append("},");
		inOnEditedEventHandler = false;
	}

	// Used to sort out server-side events into the bizEditedForServer() method.
	private boolean inOnRemovedEventHandler = false;

	@Override
	public void visitOnRemovedEventHandler(Removable removable,
											boolean parentVisible,
											boolean parentEnabled) {
		if (dataWidgetFieldsIncomplete) {
			code.setLength(code.length() - 1);
			code.append("],");
			dataWidgetFieldsIncomplete = false;
		}
		inOnRemovedEventHandler = true;
		if (eventsWithNoForm) {
			code.append("bizRemoved:function(){var view=this._view;");
		}
		else {
			code.append("bizRemoved:function(form,item,value){var view=form._view;");
		}
	}

	@Override
	public void visitedOnRemovedEventHandler(Removable removable,
												boolean parentVisible,
												boolean parentEnabled) {
		code.append("},");
		inOnRemovedEventHandler = false;
	}

	@Override
	public void visitOnSelectedEventHandler(Selectable selectable,
												boolean parentVisible,
												boolean parentEnabled) {
		if (dataWidgetFieldsIncomplete) {
			code.setLength(code.length() - 1);
			code.append("],");
			dataWidgetFieldsIncomplete = false;
		}
		code.append("bizSelected:function(){var view=this._view;");
	}

	@Override
	public void visitedOnSelectedEventHandler(Selectable selectable,
												boolean parentVisible,
												boolean parentEnabled) {
		code.append("},");
	}

	@Override
	public void visitOnPickedEventHandler(Lookup lookup,
											boolean parentVisible,
											boolean parentEnabled) {
		code.append("bizPicked:function(form,item,value){var view=form._view;");
	}

	@Override
	public void visitedOnPickedEventHandler(Lookup lookup,
												boolean parentVisible,
												boolean parentEnabled) {
		code.append("},");
	}

	@Override
	public void visitOnClearedEventHandler(Lookup lookup,
											boolean parentVisible,
											boolean parentEnabled) {
		code.append("bizCleared:function(form,item,value){var view=form._view;");
	}

	@Override
	public void visitedOnClearedEventHandler(Lookup lookup,
												boolean parentVisible,
												boolean parentEnabled) {
		code.append("},");
	}

	private void writeOutServerSideCallbackMethodIfNecessary() {
		if (inOnAddedEventHandler) {
			code.append("},bizAddedForServer:function(form,item,value){var view=form._view;");
		}
		if (inOnEditedEventHandler) {
			code.append("},bizEditedForServer:function(form,item,value){var view=form._view;");
		}
		if (inOnRemovedEventHandler) {
			code.append("},bizRemovedForServer:function(form,item,value){var view=form._view;");
		}
	}
	
	@Override
	public void visitRerenderEventAction(RerenderEventAction rerender,
											EventSource source,
											boolean parentVisible,
											boolean parentEnabled) {
		if (! eventsWithNoForm) {
			writeOutServerSideCallbackMethodIfNecessary();
		}
		code.append(visitingOnBlur ? "view.rerenderBlurryAction(" : "view.rerenderAction(");
		code.append(Boolean.FALSE.equals(rerender.getClientValidation()) ? "false,'" : "true,'");
		code.append(source.getSource()).append("');");
	}

	@Override
	public void visitServerSideActionEventAction(ServerSideActionEventAction server,
													boolean parentVisible,
													boolean parentEnabled) {
		if (! eventsWithNoForm) {
			writeOutServerSideCallbackMethodIfNecessary();
		}
		Action action = view.getAction(server.getActionName());
		code.append(visitingOnBlur ? "view.doBlurryAction('" : "view.doAction('");
		code.append(server.getActionName()).append("',");
		code.append(! Boolean.FALSE.equals(action.getClientValidation())).append(");");
	}

	@Override
	public void visitSetDisabledEventAction(SetDisabledEventAction setDisabled,
												boolean parentVisible,
												boolean parentEnabled) {
		code.append("view.setDisabled('").append(BindUtil.sanitiseBinding(setDisabled.getBinding()));
		code.append("','").append(setDisabled.getDisabledConditionName()).append("');");
	}

	@Override
	public void visitSetInvisibleEventAction(SetInvisibleEventAction setInvisible,
												boolean parentVisible,
												boolean parentEnabled) {
		code.append("view.setInvisible('").append(BindUtil.sanitiseBinding(setInvisible.getBinding()));
		code.append("','").append(setInvisible.getInvisibleConditionName()).append("');");
	}

	@Override
	public void visitToggleDisabledEventAction(ToggleDisabledEventAction toggleDisabled,
												boolean parentVisible,
												boolean parentEnabled) {
		code.append("view.toggleDisabled('").append(BindUtil.sanitiseBinding(toggleDisabled.getBinding()));
		code.append("');");
	}

	@Override
	public void visitToggleVisibilityEventAction(ToggleVisibilityEventAction toggleVisibility,
													boolean parentVisible,
													boolean parentEnabled) {
		code.append("view.toggleVisibility('").append(BindUtil.sanitiseBinding(toggleVisibility.getBinding()));
		code.append("');");
	}

	/**
	 * This generates an ID based on the module name and document name and an incrementing number.
	 * @return
	 */
	private String IDExpression() {
		StringBuilder result = new StringBuilder(64);
		result.append('\'').append(module.getName()).append('_');
		result.append(document.getName()).append('_');
		result.append(view.getName()).append('_');
		result.append("'+").append(module.getName()).append('.').append(document.getName());
		if (ViewType.edit.toString().equals(view.getName())) {
			result.append(SmartClientWebContext.EDIT_ID_COUNTER).append("++");
		}
		else if (ViewType.create.toString().equals(view.getName())) {
			result.append(SmartClientWebContext.CREATE_ID_COUNTER).append("++");
		}
		
		return result.toString();
	}
	
	private void size(AbsoluteWidth sizable, 
						Integer defaultMinHeightInPixels,
						StringBuilder builder) {
		ShrinkWrap shrinkWrap = (sizable instanceof ShrinkWrapper) ? 
									((ShrinkWrapper) sizable).getShrinkWrap() :
									null;
		boolean widthShrinkWrapped = false;
		boolean heightShrinkWrapped = false;
									
		if (ShrinkWrap.width.equals(shrinkWrap) || ShrinkWrap.both.equals(shrinkWrap)) {
			builder.append("width:1,");
			widthShrinkWrapped = true;
		}
		else {
			Integer width = sizable.getPixelWidth();
			boolean specifiedWidth = false;
			if (width != null) {
				builder.append("width:").append(width).append(',');
				specifiedWidth = true;
			}
			else {
				if (sizable instanceof RelativeSize) {
					RelativeSize relative = (RelativeSize) sizable;
					width = relative.getPercentageWidth();
					if (width != null) {
						builder.append("width:'").append(width).append("%',");
						specifiedWidth = true;
					}
					else {
						width = relative.getResponsiveWidth();
						if (width != null) {
							builder.append("width:'");
							builder.append(LayoutUtil.responsiveWidthToPercentageWidth(width.doubleValue()));
							builder.append("%',");
							specifiedWidth = true;
						}
					}
				}
			}
			if ((! specifiedWidth) && 
					(visitedItem != null) && 
					(! (sizable instanceof ContentSpecifiedWidth))) {
				builder.append("width:'*',");
			}
		}
		
		if (sizable instanceof AbsoluteSize) {
			if (ShrinkWrap.height.equals(shrinkWrap) || ShrinkWrap.both.equals(shrinkWrap)) {
				builder.append("height:1,");
				heightShrinkWrapped = true;
			}
			else {
				// NB Don't use height:'*' if there is no specified height because blurbs won't 
				// layout correctly based on their content.
				// Also, it doesn't help contentImages either to put in a '*'.
				Integer height = ((AbsoluteSize) sizable).getPixelHeight();
				if (height != null) {
					builder.append("height:").append(height).append(',');
				}
				else {
					if (sizable instanceof RelativeSize) {
						height = ((RelativeSize) sizable).getPercentageHeight();
						if (height != null) {
							builder.append("height:'").append(height).append("%',");
						}
					}
				}
			}
		}
		
		// process size constraints
		if (sizable instanceof MinimumHeight) {
			if (! heightShrinkWrapped) {
				Integer minHeight = ((MinimumHeight) sizable).getMinPixelHeight();
				if (minHeight == null) {
					minHeight = defaultMinHeightInPixels;
				}
				if (minHeight != null) {
					builder.append("minHeight:").append(minHeight).append(',');
				}
			}
			if (sizable instanceof ConstrainableHeight) {
				if (! heightShrinkWrapped) {
					Integer maxHeight = ((ConstrainableHeight) sizable).getMaxPixelHeight();
					if (maxHeight != null) {
						builder.append("maxHeight:").append(maxHeight).append(',');
					}
				}
				if (! widthShrinkWrapped) {
					if (sizable instanceof ConstrainableSize) {
						ConstrainableSize constrainable = (ConstrainableSize) sizable;
						Integer minWidth = constrainable.getMinPixelWidth();
						if (minWidth != null) {
							builder.append("minWidth:").append(minWidth).append(',');
						}
						Integer maxWidth = constrainable.getMaxPixelWidth();
						if (maxWidth != null) {
							builder.append("maxWidth:").append(maxWidth).append(',');
						}
					}
				}
			}
		}
	}
	
	private static void disableCRUD(DisableableCRUDGrid grid, StringBuilder builder) {
		String disabledCRUDCondition = grid.getDisableAddConditionName();
		if (disabledCRUDCondition != null) {
			builder.append("disableAddConditionName:'").append(disabledCRUDCondition).append("',");
		}
		disabledCRUDCondition = grid.getDisableZoomConditionName();
		if (disabledCRUDCondition != null) {
			builder.append("disableZoomConditionName:'").append(disabledCRUDCondition).append("',");
		}
		disabledCRUDCondition = grid.getDisableEditConditionName();
		if (disabledCRUDCondition != null) {
			builder.append("disableEditConditionName:'").append(disabledCRUDCondition).append("',");
		}
		disabledCRUDCondition = grid.getDisableRemoveConditionName();
		if (disabledCRUDCondition != null) {
			builder.append("disableRemoveConditionName:'").append(disabledCRUDCondition).append("',");
		}
	}

	private static void disableLookupComponents(Lookup lookup, StringBuilder builder) {
		String disabledCondition = lookup.getDisablePickConditionName();
		if (disabledCondition != null) {
			builder.append("disablePickConditionName:'").append(disabledCondition).append("',");
		}
		disabledCondition = lookup.getDisableEditConditionName();
		if (disabledCondition != null) {
			builder.append("disableEditConditionName:'").append(disabledCondition).append("',");
		}
		disabledCondition = lookup.getDisableAddConditionName();
		if (disabledCondition != null) {
			builder.append("disableAddConditionName:'").append(disabledCondition).append("',");
		}
		disabledCondition = lookup.getDisableClearConditionName();
		if (disabledCondition != null) {
			builder.append("disableClearConditionName:'").append(disabledCondition).append("',");
		}
	}

	private void bordered(Bordered bordered, Integer definedPixelPadding, StringBuilder builder) {
		if (Boolean.TRUE.equals(bordered.getBorder())) {
			String borderTitle = bordered.getBorderTitle();
			builder.append("styleName:'bizhubRoundedBorder',groupBorderCSS:'1px solid #bfbfbf',isGroup:true,margin:1,groupLabelBackgroundColor:'transparent',");
			if (borderTitle != null) {
				builder.append("groupTitle:'&nbsp;&nbsp;").append(SmartClientGenerateUtils.processString(Util.i18n(borderTitle, locale)));
				builder.append("&nbsp;&nbsp;',groupLabelStyleName:'bizhubBorderLabel',");
			}
			if (definedPixelPadding == null) {
				builder.append("layoutMargin:10,");
			}
		}
	}

	private static void disabled(String disabledConditionName, StringBuilder builder) {
		if (disabledConditionName != null) {
			builder.append("disabledConditionName:'").append(disabledConditionName).append("',");
		}
	}

	private static void invisible(String invisibleConditionName, StringBuilder builder) {
		if (invisibleConditionName != null) {
			builder.append("invisibleConditionName:'").append(invisibleConditionName).append("',");
		}
	}

	private static void editable(Boolean editable, StringBuilder builder) {
		builder.append("editable:").append((! Boolean.FALSE.equals(editable)) ? "true," : "false,");
	}

	private static void removeTrailingComma(StringBuilder builder) {
		int length = builder.length();
		if (builder.charAt(length - 1) == ',') {
			builder.setLength(length - 1);
		}
	}

	private void addAction(String resourceName,
							ImplicitActionName implicitName,
							String displayName,
							Boolean inActionPanel,
							Boolean clientValidation,
							String relativeIconFileName,
							String iconStyleClass,
							String tooltip,
							String confirmationText,
							List<Parameter> parameters,
							String disabledConditionName,
							String invisibleConditionName) {
		if (! Boolean.FALSE.equals(inActionPanel) && 
				(! ImplicitActionName.Add.equals(implicitName)) &&
				(! ImplicitActionName.Edit.equals(implicitName))) {
			String buttonCode = generateButton(resourceName,
												implicitName,
												displayName,
												clientValidation,
												relativeIconFileName,
												iconStyleClass,
												tooltip,
												confirmationText,
												parameters,
												disabledConditionName,
												invisibleConditionName,
												null);
			if (buttonCode != null) { // we have access
				// use double quote string delimiter to allow &quot; HTML character entity
				code.append("view.add");
				if (! noCreateView) {
					code.append(ViewType.edit.toString().equals(view.getName()) ? "Edit" : "Create");
				}
				code.append("Action(");
				code.append(buttonCode).append(");");
			}
		}
	}

	// return null if the button should NOT be added
	private String generateButton(String resourceName,
									ImplicitActionName implicitName,
									String displayName,
									Boolean clientValidation,
									String relativeIconFileName,
									String iconStyleClass,
									String tooltip,
									String confirmationText,
									List<Parameter> parameters,
									String disabledConditionName,
									String invisibleConditionName,
									Button button) { // null if called from an action defn
		StringBuilder result = new StringBuilder(128);
		String revisedRelativeIconFileName = relativeIconFileName;
		char actionType = ' ';
		if (implicitName == null) {
			if (! user.canExecuteAction(document, resourceName)) {
				return null; // cannot execute this action
			}
			result.append("isc.BizButton.create({validate:");
			result.append(! Boolean.FALSE.equals(clientValidation));
			result.append(",actionName:'").append(resourceName);
		}
		else {
			result.append("isc.BizButton.create({validate:");
			result.append(! Boolean.FALSE.equals(clientValidation));
			result.append(",actionName:'");
			switch (implicitName) {
			case Add:
				if (! user.canCreateDocument(document)) {
					return null;
				}
				if (revisedRelativeIconFileName == null) {
					revisedRelativeIconFileName = "actions/Add.gif";
				}
				actionType = 'A';
				result.append(implicitName);
				break;
			case BizExport:
				if (! user.canExecuteAction(document, resourceName)) {
					return null;
				}
				if (revisedRelativeIconFileName == null) {
					revisedRelativeIconFileName = "actions/BizExport.png";
				}
				actionType = 'X';
				result.append(resourceName);
				break;
			case BizImport:
				if (! user.canExecuteAction(document, resourceName)) {
					return null;
				}
				if (revisedRelativeIconFileName == null) {
					revisedRelativeIconFileName = "actions/BizImport.png";
				}
				actionType = 'I';
				result.append(resourceName);
				break;
			case Download:
				if (! user.canExecuteAction(document, resourceName)) {
					return null; // cannot execute this action
				}
				if (revisedRelativeIconFileName == null) {
					revisedRelativeIconFileName = "actions/Download.png";
				}
				actionType = 'L';
				result.append(resourceName);
				break;
			case Upload:
				if (! user.canExecuteAction(document, resourceName)) {
					return null; // cannot execute this action
				}
				if (revisedRelativeIconFileName == null) {
					revisedRelativeIconFileName = "actions/Upload.png";
				}
				actionType = 'U';
				result.append(resourceName);
				break;
			case Cancel:
				if (revisedRelativeIconFileName == null) {
					revisedRelativeIconFileName = "actions/Cancel.gif";
				}
				actionType = 'C';
				result.append(implicitName);
				break;
			case Delete:
				if (! user.canDeleteDocument(document)) {
					return null;
				}
				if (revisedRelativeIconFileName == null) {
					revisedRelativeIconFileName = "actions/Delete.gif";
				}
				actionType = 'D';
				result.append(implicitName);
				break;
			case Edit:
				if (! user.canReadDocument(document)) {
					return null;
				}
				if (revisedRelativeIconFileName == null) {
					revisedRelativeIconFileName = "actions/Edit.gif";
				}
				actionType = 'E';
				result.append(implicitName);
				break;
			case New:
				if (! user.canCreateDocument(document)) {
					return null;
				}
				if (revisedRelativeIconFileName == null) {
					revisedRelativeIconFileName = "actions/New.gif";
				}
				actionType = 'N';
				result.append(implicitName);
				break;
			case OK:
				if ((! user.canUpdateDocument(document)) && 
						(! user.canCreateDocument(document))) {
					return null;
				}
				if (revisedRelativeIconFileName == null) {
					revisedRelativeIconFileName = "actions/OK.gif";
				}
				actionType = 'O';
				result.append(implicitName);
				break;
			case Remove:
				if (! user.canDeleteDocument(document)) {
					return null;
				}
				if (revisedRelativeIconFileName == null) {
					revisedRelativeIconFileName = "actions/Remove.gif";
				}
				actionType = 'R';
				result.append(implicitName);
				break;
			case Report:
				if (revisedRelativeIconFileName == null) {
					revisedRelativeIconFileName = "actions/Report.gif";
				}
				actionType = 'P';
				result.append(implicitName);
				break;
			case Save:
				if ((! user.canUpdateDocument(document)) && 
						(! user.canCreateDocument(document))) {
					return null;
				}
				if (revisedRelativeIconFileName == null) {
					revisedRelativeIconFileName = "actions/Save.gif";
				}
				actionType = 'S';
				result.append(implicitName);
				break;
			case ZoomOut:
				if (revisedRelativeIconFileName == null) {
					revisedRelativeIconFileName = "actions/ZoomOut.gif";
				}
				actionType = 'Z';
				result.append(implicitName);
				break;
			case Print:
				if (revisedRelativeIconFileName == null) {
					revisedRelativeIconFileName = "actions/Report.gif";
				}
				actionType = 'V';
				result.append(implicitName);
				break;
			default:
				throw new IllegalArgumentException(implicitName + " not catered for");
			}
		}
		result.append("',type:'");
		result.append(actionType);
		if (revisedRelativeIconFileName != null) {
			result.append("',icon:'../resources?_doc=");
			result.append(module.getName()).append('.').append(document.getName());
			result.append("&_n=").append(revisedRelativeIconFileName);
			result.append("',displayName:'");
		}
		else {
			result.append("',displayName:'");
			if (iconStyleClass != null) {
				result.append("<i class=\"bizhubFontIcon ").append(iconStyleClass).append("\"></i>");
			}
		}
		result.append((displayName == null) ? ((implicitName == null) ? " " : Util.i18n(implicitName.getDisplayName(), locale)) : SmartClientGenerateUtils.processString(Util.i18n(displayName, locale)));
		result.append("',tabIndex:999,");
		if (button != null) {
			size(button, null, result);
		}
		disabled(disabledConditionName, result);
		invisible(invisibleConditionName, result);
		if (tooltip != null) {
			result.append("tooltip:'").append(SmartClientGenerateUtils.processString(Util.i18n(tooltip, locale))).append("',");
		}
		if (confirmationText != null) {
			result.append("confirm:'").append(SmartClientGenerateUtils.processString(Util.i18n(confirmationText, locale))).append("',");
		}
		appendParameters(parameters, result);
		result.append("_view:view})");
		
		return result.toString();
	}

	private static void appendParameters(List<Parameter> parameters, StringBuilder builder) {
		if ((parameters != null) && (! parameters.isEmpty())) {
			builder.append("params:{");
			for (Parameter parameter : parameters) {
				builder.append("'").append(BindUtil.sanitiseBinding(parameter.getName())).append("':'");
				String binding = parameter.getBinding();
				if (binding != null) {
					builder.append('{').append(binding).append("}',");
				}
				else {
					builder.append(parameter.getValue()).append("',");
				}
			}
			builder.setLength(builder.length() - 1); // remove comma
			builder.append("},");
		}
	}

	private static void appendFilterParameters(List<FilterParameter> parameters, StringBuilder builder) {
		if ((parameters != null) && (! parameters.isEmpty())) {
			builder.append("params:[");
			for (FilterParameter parameter : parameters) {
				builder.append("{name:'").append(BindUtil.sanitiseBinding(parameter.getName())).append("',operator:'");
				builder.append(SmartClientFilterOperator.fromFilterOperator(parameter.getOperator())).append("',value:'");
				String binding = parameter.getBinding();
				if (binding != null) {
					builder.append('{').append(binding).append("}'},");
				}
				else {
					builder.append(parameter.getValue()).append("'},");
				}
			}
			builder.setLength(builder.length() - 1); // remove comma
			builder.append("],");
		}
	}

	private SmartClientFieldDefinition preProcessFormItem(InputWidget widget,
															String typeOverride) {
		SmartClientFieldDefinition def = SmartClientGenerateUtils.getField(user,
																			customer,
																			module,
																			document,
																			widget);
		if (typeOverride != null) {
			def.setType(typeOverride);
		}
		String title = (visitedItem == null) ? null : visitedItem.getLabel();
		if (title != null) {
			def.setTitle(Util.i18n(title, locale));
		}
		Boolean required = (visitedItem == null) ? null : visitedItem.getRequired();
		if (required != null) {
			def.setRequired(required.booleanValue());
		}
		
		code.append(def.toJavascript());
		code.append(',');

		return def;
	}
}
