package org.skyve.impl.web.service.smartclient;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Stack;
import java.util.TreeMap;
import java.util.TreeSet;

import org.skyve.domain.Bean;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.generate.ViewRenderer;
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
import org.skyve.impl.metadata.view.LoadingType;
import org.skyve.impl.metadata.view.MinimumHeight;
import org.skyve.impl.metadata.view.RelativeSize;
import org.skyve.impl.metadata.view.RelativeWidth;
import org.skyve.impl.metadata.view.ShrinkWrap;
import org.skyve.impl.metadata.view.ShrinkWrapper;
import org.skyve.impl.metadata.view.VerticalAlignment;
import org.skyve.impl.metadata.view.container.Box;
import org.skyve.impl.metadata.view.container.Collapsible;
import org.skyve.impl.metadata.view.container.HBox;
import org.skyve.impl.metadata.view.container.Sidebar;
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
import org.skyve.impl.metadata.view.widget.Chart;
import org.skyve.impl.metadata.view.widget.DialogButton;
import org.skyve.impl.metadata.view.widget.DynamicImage;
import org.skyve.impl.metadata.view.widget.Link;
import org.skyve.impl.metadata.view.widget.MapDisplay;
import org.skyve.impl.metadata.view.widget.Spacer;
import org.skyve.impl.metadata.view.widget.StaticImage;
import org.skyve.impl.metadata.view.widget.bound.Label;
import org.skyve.impl.metadata.view.widget.bound.ProgressBar;
import org.skyve.impl.metadata.view.widget.bound.ZoomIn;
import org.skyve.impl.metadata.view.widget.bound.input.CheckBox;
import org.skyve.impl.metadata.view.widget.bound.input.CheckMembership;
import org.skyve.impl.metadata.view.widget.bound.input.ColourPicker;
import org.skyve.impl.metadata.view.widget.bound.input.Combo;
import org.skyve.impl.metadata.view.widget.bound.input.Comparison;
import org.skyve.impl.metadata.view.widget.bound.input.CompleteType;
import org.skyve.impl.metadata.view.widget.bound.input.ContentImage;
import org.skyve.impl.metadata.view.widget.bound.input.ContentLink;
import org.skyve.impl.metadata.view.widget.bound.input.ContentSignature;
import org.skyve.impl.metadata.view.widget.bound.input.Geometry;
import org.skyve.impl.metadata.view.widget.bound.input.GeometryInputType;
import org.skyve.impl.metadata.view.widget.bound.input.GeometryMap;
import org.skyve.impl.metadata.view.widget.bound.input.HTML;
import org.skyve.impl.metadata.view.widget.bound.input.InputWidget;
import org.skyve.impl.metadata.view.widget.bound.input.ListMembership;
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
import org.skyve.impl.snapshot.SmartClientFilterOperator;
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
import org.skyve.metadata.module.query.MetaDataQueryColumn;
import org.skyve.metadata.module.query.MetaDataQueryContentColumn;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.module.query.MetaDataQueryProjectedColumn;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.Action;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.metadata.view.widget.FilterParameter;
import org.skyve.metadata.view.widget.bound.Parameter;
import org.skyve.util.Binder.TargetMetaData;
import org.skyve.util.OWASP;
import org.skyve.util.Util;

import jakarta.annotation.Nonnull;

public class SmartClientViewRenderer extends ViewRenderer {
	private static final Integer DEFAULT_MIN_HEIGHT_IN_PIXELS = Integer.valueOf(170);
	private static final Integer DEFAULT_TAB_MIN_HEIGHT_IN_PIXELS = Integer.valueOf(200);

	private boolean noCreateView;
	private int variableCounter = 0;

	// This is used to assign names to boilerplate values which have binding expressions in them, such as links.
	// These values are evaluated server side and stashed in the JSON response under a bogus binding.
	private int formatCounter = 0;

	private StringBuilder code = new StringBuilder(2048);
	private Stack<String> containerVariables = new Stack<>();
	
	protected SmartClientViewRenderer(User user,
										Module module,
										Document document,
										View view,
										String uxui,
										boolean noCreateView) {
		super(user, module, document, view, uxui);
		this.noCreateView = noCreateView;
	}

	public StringBuilder getCode() {
		return code;
	}

	@Override
	public void renderView(String icon16x16Url, String icon32x32Url) {
		UtilImpl.LOGGER.info("VIEW = " + view.getTitle() + " for " + document.getName());
		Sidebar sidebar = view.getSidebar();
		if (noCreateView) {
			if (sidebar == null) {
				containerVariables.push("view");
			}
			else {
				rearrangeForSidebar(sidebar, null, "edit");
			}
		}
		else if (ViewType.edit.toString().equals(view.getName())) {
			if (sidebar == null) {
				code.append("var edit=isc.BizContainer.create({width:'100%',height:'100%',invisibleConditionName:'");
				code.append(Bean.NOT_CREATED_KEY);
				code.append("'});");
				containerVariables.push("edit");
			}
			else {
				rearrangeForSidebar(sidebar, Bean.NOT_CREATED_KEY, "edit");
			}
		}
		else if (ViewType.create.toString().equals(view.getName())) {
			if (sidebar == null) {
				code.append("var create=isc.BizContainer.create({width:'100%',height:'100%',invisibleConditionName:'");
				code.append(Bean.CREATED_KEY);
				code.append("'});");
				containerVariables.push("create");
			}
			else {
				rearrangeForSidebar(sidebar, Bean.CREATED_KEY, "create");
			}
		}
	}

	private void rearrangeForSidebar(Sidebar sidebar, String invisibleConditionName, String variableName) {
		code.append("var sidebarPane=isc.BizContainer.create({");
		size(sidebar, null, code);
		invisible(sidebar.getInvisibleConditionName(), code);
		// Override hide/show to switch on and off the resize bar in the viewPane.
		// Skyve calls Canvas.hide()/Canvas.show() directly when processing the widget tree on scatter, whereas the Snapbar collapse function calls BizHBox.hideMember()
		// We set the _hiding variable so we know when we are collapsing in the UI versus programmatically hiding on invisible condition
		code.append("hide:function(){this.getParentCanvas().getMember(0).setShowResizeBar(this._hiding||false);this._hiding=false;this.Super('hide',arguments);},");
		code.append("show:function(){this.getParentCanvas().getMember(0).setShowResizeBar(this._hiding||true);this.Super('show',arguments);},");
		code.append("height:'100%',padding:5,shadowSoftness:10,shadowOffset:0,showShadow:true});");
		
		code.append("var viewPane=isc.BizContainer.create({width:'*',height:'100%',padding:5,shadowSoftness:10,shadowOffset:0,showShadow:true,showResizeBar:true,resizeBarTarget:'next'});");
		containerVariables.push("viewPane");
		
		code.append("var ").append(variableName).append("=isc.BizHBox.create({width:'100%',height:'100%',padding:10,");
		invisible(invisibleConditionName, code);
		// Override hideMember to indicate that we are using the Snapbar collapse function
		// Skyve calls Canvas.hide()/Canvas.show() directly when processing the widget tree on scatter, whereas the Snapbar collapse function calls BizHBox.hideMember()
		// We set the _hiding variable so we know when we are collapsing in the UI versus programmatically hiding on invisible condition
		code.append("hideMember: function(member, callback) {member._hiding = true;this.Super('hideMember', arguments);}");
		code.append("});\n");
		code.append(variableName).append(".addContained(viewPane);\n");
		code.append(variableName).append(".addContained(sidebarPane);\n");
				
		containerVariables.push("viewPane");

	}
	
	@Override
	public void renderedView(String icon16x16Url, String icon32x32Url) {
		containerVariables.pop();
		if (noCreateView) {
			if (view.getSidebar() != null) {
				code.append("view.addContained(edit);");
			}
		}
		else {
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

	// This is a stack in case we have a tab pane inside a tab pane
	private Stack<Integer> tabNumbers = new Stack<>();

	@Override
	public void renderTabPane(TabPane tabPane) {
		tabNumbers.push(Integer.valueOf(0));
		String variable = "v" + variableCounter++;
		code.append("var ").append(variable).append("=isc.BizTabPane.create({");
		size(tabPane, DEFAULT_TAB_MIN_HEIGHT_IN_PIXELS, code);
		disabled(tabPane.getDisabledConditionName(), code);
		invisible(tabPane.getInvisibleConditionName(), code);
		String selected = tabPane.getSelectedTabIndexBinding();
		if (selected != null) {
			code.append("selectedTabIndexBinding:'").append(BindUtil.sanitiseBinding(selected)).append("',");
		}
		code.append("_view:view});\n");

		containerVariables.push(variable);
	}

	@Override
	public void renderedTabPane(TabPane tabPane) {
		String variable = containerVariables.pop();
		code.append(containerVariables.peek()).append(".addContained(").append(variable).append(");\n");
		tabNumbers.pop();
	}

	@Override
	public void renderTab(String title, String icon16x16Url, Tab tab) {
		String variable = "v" + variableCounter++;
		code.append("var ").append(variable).append("=isc.BizContainer.create({membersMargin:10,layoutMargin:10});\n");

		containerVariables.push(variable);
	}

	@Override
	public void renderedTab(String title, String icon16x16Url, Tab tab) {
		String paneVariable = containerVariables.pop();
		String tabPaneVariable = containerVariables.peek();
		Integer tabNumber = tabNumbers.pop();
		code.append(tabPaneVariable).append(".addBizTab({name:'").append(tabNumber);
		String iconStyleClass = tab.getIconStyleClass();
		if (iconStyleClass != null) {
			code.append("',title:'").append("<i class=\"bizhubFontIcon ").append(iconStyleClass).append("\"></i><span> &nbsp;</span>");
		}
		else if (icon16x16Url != null) {
			code.append("',icon:'../").append(icon16x16Url);
			code.append("',title:'");
		}
		else {
			code.append("',title:'");
		}

		code.append(OWASP.escapeJsString(title));
		code.append("',pane:").append(paneVariable).append(',');
		tabNumbers.push(Integer.valueOf(tabNumber.intValue() + 1));
		disabled(tab.getDisabledConditionName(), code);
		invisible(tab.getInvisibleConditionName(), code);
		removeTrailingComma(code);
		code.append("});\n");
	}

	@Override
	public void renderVBox(String borderTitle, VBox vbox) {
		vbox(borderTitle, vbox, false);
	}

	private void vbox(String borderTitle, VBox vbox, boolean forFormBorder) {
		String variable = "v" + variableCounter++;
		code.append("var ").append(variable).append("=isc.BizVBox.create({");

		// if collapsible, then make the inner vbox 100% width and height and do not put the border/title
		Collapsible collapsible = vbox.getCollapsible();
		if (collapsible != null) {
			validateCollapsible(collapsible, borderTitle);
			code.append("width:'100%',height:'100%',");
		}
		else {
			size(vbox, null, code);
			bordered(borderTitle, vbox, vbox.getPixelPadding(), code);
		}
		
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

		String collapsibleVar = collapsible(borderTitle, vbox, variable, forFormBorder);
		code.append(containerVariables.peek()).append(".addContained(").append((collapsibleVar == null) ? variable : collapsibleVar).append(");\n");
		containerVariables.push(variable);
	}
	
	@Override
	public void renderedVBox(String borderTitle, VBox vbox) {
		containerVariables.pop();
	}

	@Override
	public void renderHBox(String borderTitle, HBox hbox) {
		String variable = "v" + variableCounter++;
		code.append("var ").append(variable).append("=isc.BizHBox.create({");
		
		// if collapsible, then make the inner hbox 100% width and height and do not put the border/title
		Collapsible collapsible = hbox.getCollapsible();
		if (collapsible != null) {
			code.append("width:'100%',height:'100%',");
			validateCollapsible(collapsible,borderTitle);
		}
		else {
			size(hbox, null, code);
			bordered(borderTitle, hbox, hbox.getPixelPadding(), code);
		}

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
		box(hbox);
		invisible(hbox.getInvisibleConditionName(), code);
		removeTrailingComma(code);
		
		code.append("});\n");

		String collapsibleVar = collapsible(borderTitle, hbox, variable, false);
		code.append(containerVariables.peek()).append(".addContained(").append((collapsibleVar == null) ? variable : collapsibleVar).append(");\n");
		containerVariables.push(variable);
	}
	
	@Override
	public void renderedHBox(String title, HBox bbox) {
		containerVariables.pop();
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
	
	private String collapsible(String borderTitle, Box box, String itemVariable, boolean autoSize) {
		String result = null;
		Collapsible collapsible = box.getCollapsible();
		if (collapsible != null) {
			result = "v" + variableCounter++;
			code.append("var ").append(result).append("=isc.BizCollapsible.create({title:'").append(OWASP.escapeJsonString(borderTitle));
			code.append("',minimized:").append(collapsible.equals(Collapsible.closed) ? "true" : "false");
			code.append(",autoSize:").append(autoSize).append(',');
			size(box, null, code);
			invisible(box.getInvisibleConditionName(), code);
			removeTrailingComma(code);
			code.append("});\n");
			code.append(result).append(".addContained(").append(itemVariable).append(");\n");
		}
		return result;
	}
	
	private boolean viewHasAtLeastOneForm = false;
	private String formVariable = null;
	private VBox borderBox = null;

	@Override
	public void renderForm(String borderTitle, Form form) {
		viewHasAtLeastOneForm = true;

		// If a form is defined with a border, then wrap the form definition in a vbox.
		// SC 8.2 couldn't cope with chrome and would draw the fieldset/border too small
		// for its content.
		Boolean border = form.getBorder();
		Collapsible collapsible = form.getCollapsible();
		
		validateCollapsible(collapsible,borderTitle);
		
		if ((collapsible != null) || Boolean.TRUE.equals(border)) {
			borderBox = new VBox();
			borderBox.setBorder(Boolean.TRUE);
			borderBox.setBorderTitle(form.getLocalisedBorderTitle());
			borderBox.setCollapsible(collapsible);
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
			
			vbox(borderTitle, borderBox, true);
		}
		
		formVariable = "v" + variableCounter++;
		code.append("var ").append(formVariable);
		code.append("=isc.DynamicForm.create({longTextEditorType:'text',longTextEditorThreshold:102400,");
		// Render form with top labels if required (increase cell padding somewhat to accommodate
		if (isCurrentFormRenderTopLabels()) {
			code.append("titleOrientation:'top',cellPadding:5,");
		}
		// SC docs says that autoFocus will focus in first focusable item
		// in the form when it is drawn.
		// Don't use autoFocus as we have multiple dynamic forms that can be declared
		// in some views which doesn't work.
		code.append("_view:view,");
		code.append("ID:").append(IDExpression()).append(',');
		disabled(form.getDisabledConditionName(), code);
//code.append("cellBorder:1,");

		// only size the form if its not in a border VBox
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
	public void renderedForm(String borderTitle, Form form) {
		code.setLength(code.length() - 1); // remove last comma
		code.append("]);\n");
		code.append(containerVariables.peek()).append(".addContained(").append(formVariable).append(");\n");
		formVariable = null;
		renderedFormRow = false;

		if (Boolean.TRUE.equals(form.getBorder())) {
			renderedVBox(borderTitle, borderBox);
			borderBox = null;
		}
	}

	@Override
	public void renderFormColumn(FormColumn column) {
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

	// have we rendered a form row in this form yet
	private boolean renderedFormRow = false;
	// have we started a new row
	private boolean startedNewFormRow = false;

	@Override
	public void renderFormRow(FormRow row) {
		startedNewFormRow = true;
		if (! renderedFormRow) {
			code.setLength(code.length() - 1); // remove last column comma
			code.append("]});");
			code.append("view._vm.addMember(").append(formVariable).append(");\n");
			code.append(formVariable).append(".setItems([");
		}

		renderedFormRow = true;
	}

	@Override
	public void renderFormItem(String label,
								boolean required,
								String help,
								boolean showLabel,
								int colspan,
								FormItem item) {
		code.append("{showTitle:").append(showLabel).append(',');
		// label handled in preProcessFormItem()
		if (colspan >= 1) {
			code.append("colSpan:").append(colspan).append(',');
		}
		Integer rowspan = item.getRowspan();
		if (rowspan != null) {
			code.append("rowSpan:").append(rowspan).append(',');
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
	public void renderedFormItem(String label,
									boolean required,
									String help,
									boolean showLabel,
									int colspan,
									FormItem item) {
		if (startedNewFormRow) {
			code.append("startRow:true},");
			startedNewFormRow = false;
		}
		else {
			code.append("startRow:false},");
		}

		// Move along the requisite amount of form columns
		if (showLabel && (! isCurrentFormRenderTopLabels())) {
			incrementFormColumn();
		}
		for (int i = 0, l = colspan; i < l; i++) {
			incrementFormColumn();
		}
	}

	@Override
	public void renderedFormRow(FormRow row) {
		code.setLength(code.length() - 2); // remove "},"
		code.append(",endRow:true},");
	}

	@Override
	public void renderFormButton(String name,
									String label,
									String iconUrl,
									String iconStyleClass,
									String toolTip,
									String confirmationText,
									Action action,
									Button button) {
		String buttonCode = generateButton(name,
											action.getImplicitName(),
											label,
											action.getClientValidation(),
											iconUrl,
											iconStyleClass,
											toolTip,
											confirmationText,
											action.getParameters(),
											action.getDisabledConditionName(),
											action.getInvisibleConditionName(),
											button,
											null);
		code.append("type:'canvas',showTitle:false,width:1,canvas:isc.HLayout.create({height:22,members:[");
		code.append(buttonCode).append("]}),");
		disabled(action.getDisabledConditionName(), code);
		invisible(action.getInvisibleConditionName(), code);
	}

	@Override
	public void renderButton(String name,
								String label,
								String iconUrl,
								String iconStyleClass,
								String toolTip,
								String confirmationText,
								Action action,
								Button button) {
		String buttonCode = generateButton(name,
											action.getImplicitName(),
											label,
											action.getClientValidation(),
											iconUrl,
											iconStyleClass,
											toolTip,
											confirmationText,
											action.getParameters(),
											action.getDisabledConditionName(),
											action.getInvisibleConditionName(),
											button,
											null);
		String variable = "v" + variableCounter++;
		code.append("var ").append(variable).append('=').append(buttonCode).append(";\n");
		code.append(containerVariables.peek()).append(".addContained(").append(variable).append(");\n");
	}

	@Override
	public void renderFormZoomIn(String label,
									String iconUrl,
									String iconStyleClass,
									String toolTip,
									ZoomIn zoomIn) {
		String zoomInCode = generateZoomIn(label, iconUrl, iconStyleClass, toolTip, zoomIn);
		code.append("type:'canvas',showTitle:false,width:1,canvas:");
		code.append(zoomInCode).append(',');
		disabled(zoomIn.getDisabledConditionName(), code);
		invisible(zoomIn.getInvisibleConditionName(), code);
	}

	@Override
	public void renderZoomIn(String label,
								String iconUrl,
								String iconStyleClass,
								String toolTip,
								ZoomIn zoomIn) {
		String zoomInCode = generateZoomIn(label, iconUrl, iconStyleClass, toolTip, zoomIn);
		String variable = "v" + variableCounter++;
		code.append("var ").append(variable).append('=').append(zoomInCode).append(";\n");
		code.append(containerVariables.peek()).append(".addContained(").append(variable).append(");\n");
	}

	@Override
	public void renderMap(MapDisplay map) {
		String variable = "v" + variableCounter++;
		code.append("var ").append(variable).append("=isc.BizMap.create({_view:view,loading:'");
		LoadingType loading = map.getLoading();
		code.append((loading == null) ? LoadingType.eager : loading).append("',refreshTime:");
		Integer refreshTime = map.getRefreshTimeInSeconds();
		code.append((refreshTime == null) ? 0 : refreshTime.intValue()).append(",showRefresh:");
		Boolean showRefreshControls = map.getShowRefreshControls();
		code.append((Boolean.TRUE.equals(showRefreshControls))).append("});");
		code.append(variable).append(".setDataSource('").append(map.getModelName()).append("');\n");
		code.append(containerVariables.peek()).append(".addContained(").append(variable).append(");\n");
	}

	@Override
	public void renderChart(Chart chart) {
		String variable = "v" + variableCounter++;
		code.append("var ").append(variable).append("=isc.BizChart.create({_view:view,");
		size(chart, null, code);
		code.append("chartType:'").append(chart.getType()).append("'});");
		String dataSource = chart.getModelName();
		if (dataSource == null) {
			dataSource = String.valueOf(chart.getModel().getModelName());
		}
		code.append(variable).append(".setDataSource('").append(dataSource).append("');\n");
		code.append(containerVariables.peek()).append(".addContained(").append(variable).append(");\n");
	}

	@Override
	public void renderBoundColumnGeometry(Geometry geometry) {
		dataWidgetColumnInputWidget = geometry;
	}

	@Override
	public void renderedBoundColumnGeometry(Geometry geometry) {
		// do nothing
	}

	@Override
	public void renderFormGeometry(Geometry geometry) {
		preProcessFormItem(geometry, "geometry");
		size(geometry, null, code);
		disabled(geometry.getDisabledConditionName(), code);
		invisible(geometry.getInvisibleConditionName(), code);

		// Highlight text on focus
		code.append("selectOnFocus:true,");

		GeometryInputType type = geometry.getType();
		if (type != null) {
			code.append("drawingTools:'").append(type).append("',");
		}

		// TODO add in the filter operators allowed
	}

	@Override
	public void renderedFormGeometry(Geometry geometry) {
		// do nothing
	}

	@Override
	public void renderFormGeometryMap(GeometryMap geometry) {
		preProcessFormItem(geometry, "geometryMap");

		// If no height, make the map at least 150px high so that all the map controls fit
		boolean noHeight = (geometry.getPixelHeight() == null) && (geometry.getPercentageHeight() == null) && (geometry.getMinPixelHeight() == null);
		if (noHeight) {
			geometry.setPercentageHeight(Integer.valueOf(100));
			geometry.setMinPixelHeight(Integer.valueOf(170));
		}
		size(geometry, null, code);
		if (noHeight) {
			geometry.setPercentageHeight(null);
			geometry.setMinPixelHeight(null);
		}

		disabled(geometry.getDisabledConditionName(), code);
		invisible(geometry.getInvisibleConditionName(), code);

		GeometryInputType type = geometry.getType();
		if (type != null) {
			code.append("drawingTools:'").append(type).append("',");
		}
	}

	@Override
	public void renderedFormGeometryMap(GeometryMap geometry) {
		// do nothing
	}

	@Override
	public void renderFormDialogButton(String label, DialogButton button) {
		code.append("type:'blurb',defaultValue:'dialog button ");
		code.append(OWASP.escapeJsString(label)).append("',");
		disabled(button.getDisabledConditionName(), code);
		invisible(button.getInvisibleConditionName(), code);
	}

	@Override
	public void renderDialogButton(String label, DialogButton button) {
		String variable = "v" + variableCounter++;
		code.append("var ").append(variable).append("=isc.BizLabel.create({value: '");
		code.append(OWASP.escapeJsString(label));
		code.append("'});\n");
		code.append(containerVariables.peek()).append(".addContained(").append(variable).append(");\n");
	}

	@Override
	public void renderFormSpacer(Spacer spacer) {
		code.append("type:'spacer',");
		size(spacer, null, code);
		invisible(spacer.getInvisibleConditionName(), code);
	}

	@Override
	public void renderSpacer(Spacer spacer) {
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

	// TODO size, invisibility and binding
	@Override
	public void renderFormStaticImage(String fileUrl, StaticImage image) {
		code.append("type:'canvas',showTitle:false,canvas:");
		addStaticImage(image);
		code.append(',');
	}

	@Override
	public void renderContainerColumnStaticImage(String fileUrl, StaticImage image) {
		// markup is generated in the JSON data for a data grid container column static image
	}

	// TODO size, invisibility and binding
	@Override
	public void renderStaticImage(String fileUrl, StaticImage image) {
		String variable = "v" + variableCounter++;
		code.append("var ").append(variable).append('=');
		addStaticImage(image);
		code.append(";\n");
		code.append(containerVariables.peek()).append(".addContained(").append(variable).append(");\n");
	}

	private void addStaticImage(StaticImage image) {
		code.append("isc.BizImage.create({modoc:'").append(module.getName()).append('.').append(document.getName());
		code.append("',file:'").append(image.getRelativeFile()).append("',");
		size(image, null, code);
		removeTrailingComma(code);
		code.append("})");
	}

	@Override
	public void renderContainerColumnDynamicImage(DynamicImage image) {
		// markup is generated in the JSON data for a data grid container column dynamic image
	}

	@Override
	public void renderDynamicImage(DynamicImage image) {
		String variable = "v" + variableCounter++;
		code.append("var ").append(variable).append('=');
		addImage(image);
		code.append(";\n");
		code.append(containerVariables.peek()).append(".addContained(").append(variable).append(");\n");
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

	@Override
	public void renderFormLink(String value, Link link) {
		// Take care of the title, as we're not calling preProcessFormItem
		String label = getCurrentWidgetLabel();
		if (label == null) {
			label = "Link";
		}
		code.append("title:'").append(label).append("',");
		code.append("type:'blurb',name:'_");
		code.append(formatCounter++).append("',"); // _1, _2 and so on
		size(link, null, code);
		invisible(link.getInvisibleConditionName(), code);
	}

	@Override
	public void renderContainerColumnLink(String value, Link link) {
		// markup is generated in the JSON data for a data grid container column link
	}

	@Override
	public void renderLink(String value, Link link) {
		// TODO Implement later
	}

	private static Label makeNewLabelFromBlurb(Blurb blurb) {
		Label result = new Label();
		result.setValue(blurb.getMarkup());
		result.setPixelWidth(blurb.getPixelWidth());
		result.setPixelHeight(blurb.getPixelHeight());
		result.setTextAlignment(blurb.getTextAlignment());
		result.setInvisibleConditionName(blurb.getInvisibleConditionName());
		return result;
	}

	@Override
	public void renderFormBlurb(String markup, Blurb blurb) {
		renderFormLabel(markup, BindUtil.containsSkyveExpressions(markup), makeNewLabelFromBlurb(blurb));
	}

	@Override
	public void renderContainerColumnBlurb(String markup, Blurb blurb) {
		renderContainerColumnLabel(markup, makeNewLabelFromBlurb(blurb));
	}

	@Override
	public void renderBlurb(String markup, Blurb blurb) {
		renderLabel(markup, BindUtil.containsSkyveExpressions(markup), makeNewLabelFromBlurb(blurb));
	}

	@Override
	public void renderFormLabel(String value, boolean boundValue, Label label) {
		FormItem currentFormItem = getCurrentFormItem();

		// Set colSpan 1 if not set otherwise all formatting hell breaks loose
		if (currentFormItem.getColspan() == null) { // not set
			code.append("colSpan:1,");
		}
		// Set endRow false as well to stop formatting gayness
		// Since this is not an input widget, we can't use preProcessFormItem()
		// Take care of the title, as we're not calling preProcessFormItem
		String title = currentFormItem.getLocalisedLabel();
		if (title == null) {
			title = value;
		}
		title = OWASP.escapeJsString(title);
		code.append("endRow:false");
		if (title != null) {
			code.append(",title:'").append(title).append('\'');
		}
		code.append(",type:'blurb',");

		String binding = label.getBinding();

		// does the value have binding expressions in them?
		if (boundValue) {
			binding = "_" + formatCounter++; // _1, _2 and so on
		}

		if (binding == null) {
			code.append("defaultValue:'").append(OWASP.escapeJsString(value, false, false));
		}
		else {
			code.append("name:'").append(BindUtil.sanitiseBinding(binding));
		}
		code.append("',");

		HorizontalAlignment alignment = label.getTextAlignment();
		if (alignment != null) {
			code.append("textAlign:'").append(alignment.toAlignmentString()).append("',");
		}
		size(label, null, code);
		invisible(label.getInvisibleConditionName(), code);
	}

	@Override
	public void renderContainerColumnLabel(String value, Label label) {
		// markup is generated in the JSON data for a data grid container column label or a dynamic form-based value
	}

	@Override
	public void renderLabel(String value, boolean boundValue, Label label) {
		// Throw if the value has binding expressions in them
		if (boundValue) {
			throw new MetaDataException("Label or blurb with a value of [" + label.getValue() + 
											"] contains a binding expression and must be declared within a form element or a data grid container column to be able to bind correctly");
		}

		String variable = "v" + variableCounter++;
		code.append("var ").append(variable).append("=isc.BizLabel.create({");

		size(label, null, code);
		if (label.getPixelWidth() == null) { // default to whole width
			code.append("width:'100%',");
		}

		HorizontalAlignment alignment = label.getTextAlignment();
		if (alignment != null) {
			code.append("textAlign:'").append(alignment.toAlignmentString()).append("',");
		}

		invisible(label.getInvisibleConditionName(), code);

		String binding = label.getBinding();
		if (binding == null) {
			code.append("value:'").append(OWASP.escapeJsString(value, false, false));
		}
		else {
			code.append("binding:'").append(BindUtil.sanitiseBinding(binding));
		}
		code.append("'});\n");
		code.append(containerVariables.peek()).append(".addContained(").append(variable).append(");\n");
	}

	@Override
	public void renderFormProgressBar(ProgressBar progressBar) {
		// TODO Make a value from CanvasItem.
		String variable = "v" + variableCounter++;
		code.append("var ").append(variable).append("=isc.BizLabel.create({value: '");
		code.append(progressBar.getBinding());
		code.append("'});\n");
		code.append(containerVariables.peek()).append(".addContained(").append(variable).append(");\n");
	}

	private String listWidgetVariable = null;

	@Override
	public void renderListGrid(String title, boolean aggregateQuery, ListGrid grid) {
		renderListWidget(grid);
		renderGrid(grid);
	}

	@Override
	public void renderListGridProjectedColumn(MetaDataQueryProjectedColumn column) {
		// TODO Auto-generated method stub

	}

	@Override
	public void renderListGridContentColumn(MetaDataQueryContentColumn column) {
		// TODO Auto-generated method stub

	}

	@Override
	public void renderedListGrid(String title, boolean aggregateQuery, ListGrid grid) {
		appendFilterParameters(grid.getFilterParameters(), grid.getParameters(), code);
		renderedListWidget();
	}

	@Override
	public void renderListRepeater(String title, ListRepeater repeater) {
		renderListWidget(repeater);
		code.append("isRepeater:true,");
		code.append("showColumnHeaders:").append(Boolean.TRUE.equals(repeater.getShowColumnHeaders())).append(',');
		code.append("showGrid:").append(Boolean.TRUE.equals(repeater.getShowGrid())).append(',');
	}

	@Override
	public void renderListRepeaterProjectedColumn(MetaDataQueryProjectedColumn column) {
		// TODO Auto-generated method stub

	}

	@Override
	public void renderListRepeaterContentColumn(MetaDataQueryContentColumn column) {
		// TODO Auto-generated method stub

	}

	@Override
	public void renderedListRepeater(String title, ListRepeater repeater) {
		renderedListWidget();
	}

	@Override
	public void renderTreeGrid(String title, TreeGrid grid) {
		renderListWidget(grid);
		renderGrid(grid);
		String rootBinding = grid.getRootIdBinding();
		if (rootBinding != null) {
			code.append("rootIdBinding:'").append(BindUtil.sanitiseBinding(rootBinding)).append("',");
		}
		code.append("isTree:true,");
	}

	@Override
	public void renderTreeGridProjectedColumn(MetaDataQueryProjectedColumn column) {
		// TODO Auto-generated method stub

	}

	@Override
	public void renderTreeGridContentColumn(MetaDataQueryContentColumn column) {
		// TODO Auto-generated method stub

	}

	@Override
	public void renderedTreeGrid(String title, TreeGrid grid) {
		appendFilterParameters(grid.getFilterParameters(), grid.getParameters(), code);
		renderedListWidget();
	}

	private void renderListWidget(AbstractListWidget widget) {
		String queryName = widget.getQueryName();
		String modelName = widget.getModelName();
		String dataSourceId = null;
		if (queryName != null) { // its a query
			MetaDataQueryDefinition query = module.getMetaDataQuery(queryName);
			StringBuilder ds = new StringBuilder(256);
			dataSourceId = SmartClientViewRenderer.appendDataSourceDefinition(user,
																				customer,
																				query,
																				null,
																				null,
																				currentUxUi,
																				false,
																				ds,
																				new TreeSet<>());
			code.insert(0, ds);
		}
		else {
			if (modelName != null) { // its a model
				StringBuilder ds = new StringBuilder(256);
				dataSourceId = SmartClientViewRenderer.appendDataSourceDefinition(user, 
																					customer, 
																					module, 
																					document,
																					modelName,
																					currentUxUi,
																					false,
																					ds, 
																					new TreeSet<>());
				code.insert(0, ds);
			}
			else {
				MetaDataQueryDefinition query = module.getDocumentDefaultQuery(customer, document.getName());
				StringBuilder ds = new StringBuilder(256);
				dataSourceId = SmartClientViewRenderer.appendDataSourceDefinition(user, customer, query, null, null, currentUxUi, false, ds, null);
				code.insert(0, ds);
			}
		}

		listWidgetVariable = "v" + variableCounter++;
		code.append("var ").append(listWidgetVariable).append("=isc.BizListGrid.create({");
		code.append("ID:").append(IDExpression()).append(',');
		code.append("dataSource:'").append(dataSourceId).append("',");
		code.append("name:'").append(listWidgetVariable).append("',");
		String title = widget.getLocalisedTitle();
		if (title != null) {
			border(OWASP.escapeJsString(title), null, code);
		}
		String postRefreshConditionName = widget.getPostRefreshConditionName();
		if (postRefreshConditionName != null) {
			code.append("postRefreshConditionName:'").append(postRefreshConditionName).append("',");
		}
		size(widget, DEFAULT_MIN_HEIGHT_IN_PIXELS, code);
		invisible(widget.getInvisibleConditionName(), code);
	}

	private void renderGrid(ListGrid grid) {
		code.append("contConv:").append(grid.getContinueConversation()).append(",");
		String selectedIdBinding = grid.getSelectedIdBinding();
		if (selectedIdBinding != null) {
			code.append("selectedIdBinding:'").append(BindUtil.sanitiseBinding(selectedIdBinding)).append("',");
			TargetMetaData target = BindUtil.getMetaDataForBinding(customer, module, document, selectedIdBinding);
			Attribute attribute = target.getAttribute();
			code.append("selectedIdTrackChanges:").append(((attribute != null) && attribute.isTrackChanges())).append(',');
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
		if (Boolean.FALSE.equals(grid.getShowChart())) {
			code.append("showChart:false,");
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

	private void renderedListWidget() {
		code.append("_view:view});\n");
		code.append(containerVariables.peek()).append(".addContained(").append(listWidgetVariable).append(");\n");
		listWidgetVariable = null;
	}

	private String dataWidgetVariable = null;
	private Document dataWidgetDocument = null;
	private String dataWidgetBinding = null;
	// Indicates whether the field definition array has been completed and closed off
	// Its used to ensure the last ']' is appended before adding events or closing the grid definition
	private boolean dataWidgetFieldsIncomplete = false;

	@Override
	public void renderDataGrid(String title, DataGrid grid) {
		renderDataWidget(grid);
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
			Attribute attribute = target.getAttribute();
			code.append("selectedIdTrackChanges:").append((attribute != null) && attribute.isTrackChanges()).append(',');
		}
		disabled(grid.getDisabledConditionName(), code);
		editable(grid.getEditable(), code);
		code.append("_fields:[");
	}

	@Override
	public void renderedDataGrid(String title, DataGrid grid) {
		renderedDataWidget();
	}

	@Override
	public void renderDataRepeater(String title, DataRepeater repeater) {
		renderDataWidget(repeater);
		code.append("isRepeater:true,");
		code.append("showColumnHeaders:").append(Boolean.TRUE.equals(repeater.getShowColumnHeaders()));
		code.append(",showGrid:").append(Boolean.TRUE.equals(repeater.getShowGrid()));
		code.append(",_fields:[");
	}

	@Override
	public void renderedDataRepeater(String title, DataRepeater repeater) {
		renderedDataWidget();
	}

	private void renderDataWidget(AbstractDataWidget widget) {
		dataWidgetBinding = widget.getBinding();
		Relation relation = (Relation) getCurrentTarget().getAttribute();
		if (relation == null) { // should never happen
			throw new MetaDataException(dataWidgetBinding + " does not point to a relation");
		}
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
		String title = widget.getLocalisedTitle();
		if (title != null) {
			border(OWASP.escapeJsString(title), null, code);
		}
		if ((relation instanceof Collection) && Boolean.TRUE.equals(((Collection) relation).getOrdered())) {
			code.append("_ordinal:'").append(Bean.ORDINAL_NAME).append("',");
		}
		size(widget, DEFAULT_MIN_HEIGHT_IN_PIXELS, code);
		invisible(widget.getInvisibleConditionName(), code);
		dataWidgetFieldsIncomplete = true;
	}

	private void renderedDataWidget() {
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
	}

	private InputWidget dataWidgetColumnInputWidget;

	@Override
	public void renderDataGridBoundColumn(String title, DataGridBoundColumn column) {
		// do nothing
	}

	@Override
	public void renderDataRepeaterBoundColumn(String title, DataGridBoundColumn column) {
		renderDataGridBoundColumn(title, column);
	}

	@Override
	public void renderedDataGridBoundColumn(String title, DataGridBoundColumn column) {
		if (dataWidgetColumnInputWidget != null) {
			SmartClientDataGridFieldDefinition def = null;
			String binding = column.getBinding();
			boolean hasFormatter = (column.getFormatterName() != null) || (column.getCustomFormatterName() != null);
			if (binding == null) { // column bound to collection for the grid
				def = getDataGridField(document, dataWidgetColumnInputWidget, dataWidgetBinding, hasFormatter, true);
			}
			else {
				def = getDataGridField(dataWidgetDocument, dataWidgetColumnInputWidget, null, hasFormatter, true);
			}

			def.setTitle(title);
			def.setEditable(! Boolean.FALSE.equals(column.getEditable()));
			def.setEscape(! Boolean.FALSE.equals(column.getEscape()));

			// NB - Text alignment and pixel width defaults are set in SmartClientDataGridFieldDefinition constructor
			HorizontalAlignment columnAlignment = column.getAlignment();
			if (columnAlignment != null) {
				def.setAlign(columnAlignment);
			}
			Integer pixelWidth = column.getPixelWidth();
			if (pixelWidth != null) {
				def.setPixelWidth(pixelWidth);
			}

			code.append('{').append(def.toJavascript()).append("},");

			SmartClientLookupDefinition lookup = def.getLookup();
			if (lookup != null) {
				StringBuilder ds = new StringBuilder(64);
				String optionDataSource = lookup.getOptionDataSource();
				SmartClientViewRenderer.appendDataSourceDefinition(user,
																	customer, 
																	lookup.getQuery(),
																	optionDataSource,
																	(LookupDescription) dataWidgetColumnInputWidget, 
																	currentUxUi,
																	false,
																	ds,
																	new TreeSet<>());
				code.insert(0, ds);
			}
			dataWidgetColumnInputWidget = null;
		}
	}

	@Override
	public void renderedDataRepeaterBoundColumn(String title, DataGridBoundColumn column) {
		renderedDataGridBoundColumn(title, column);
	}

	@Override
	public void renderDataGridContainerColumn(String title, DataGridContainerColumn column) {
		code.append("{name:'_").append(formatCounter++);
		code.append("',type:'text',formatCellValue:'value;',canEdit:false,title:'");

		code.append((title == null) ? " " : OWASP.escapeJsString(title)).append('\'');
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
	public void renderDataRepeaterContainerColumn(String title, DataGridContainerColumn column) {
		renderDataGridContainerColumn(title, column);
	}

	@Override
	public void renderedDataGridContainerColumn(String title, DataGridContainerColumn column) {
		// do nothing
	}

	@Override
	public void renderedDataRepeaterContainerColumn(String title, DataGridContainerColumn column) {
		renderedDataGridContainerColumn(title, column);
	}

	@Override
	public void renderBoundColumnCheckBox(CheckBox checkBox) {
		dataWidgetColumnInputWidget = checkBox;
	}

	@Override
	public void renderFormCheckBox(CheckBox checkBox) {
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
	public void renderedBoundColumnCheckBox(CheckBox checkBox) {
		// do nothing
	}

	@Override
	public void renderedFormCheckBox(CheckBox checkBox) {
		// do nothing
	}

	// TODO implement this - does this need size? probably
	@Override
	public void renderCheckMembership(CheckMembership membership) {
		String variable = "v" + variableCounter++;
		code.append("var ").append(variable).append("=isc.BizLabel.create({value:'");
		code.append("check membership").append(membership.getBinding());
		code.append("'});\n");
		code.append(containerVariables.peek()).append(".addContained(").append(variable).append(");\n");
	}

	@Override
	public void renderedCheckMembership(CheckMembership membership) {
		// do nothing - until implemented properly
	}

	@Override
	public void renderBoundColumnColourPicker(ColourPicker colour) {
		dataWidgetColumnInputWidget = colour;
	}

	@Override
	public void renderFormColourPicker(ColourPicker colour) {
		preProcessFormItem(colour, "color");
		size(colour, null, code);
		disabled(colour.getDisabledConditionName(), code);
		invisible(colour.getInvisibleConditionName(), code);
	}

	@Override
	public void renderedBoundColumnColourPicker(ColourPicker colour) {
		// do nothing
	}

	@Override
	public void renderedFormColourPicker(ColourPicker colour) {
		// do nothing
	}

	@Override
	public void renderBoundColumnCombo(Combo combo) {
		dataWidgetColumnInputWidget = combo;
	}

	@Override
	public void renderFormCombo(Combo combo) {
		preProcessFormItem(combo, "select");
		size(combo, null, code);
		disabled(combo.getDisabledConditionName(), code);
		invisible(combo.getInvisibleConditionName(), code);
	}

	@Override
	public void renderedBoundColumnCombo(Combo combo) {
		// do nothing
	}

	@Override
	public void renderedFormCombo(Combo combo) {
		// do nothing
	}

	@Override
	public void renderBoundColumnContentImage(ContentImage image) {
		dataWidgetColumnInputWidget = image;
	}

	@Override
	public void renderContainerColumnContentImage(ContentImage image) {
		// markup is generated in the JSON data for a data grid container column content image
	}

	@Override
	public void renderFormContentImage(ContentImage image) {
		preProcessFormItem(image, "bizContentImage");
		size(image, null, code);
		disabled(image.getDisabledConditionName(), code);
		invisible(image.getInvisibleConditionName(), code);
		editable(image.getEditable(), code);
		code.append("showMarkup:").append((! Boolean.FALSE.equals(image.getShowMarkup())) ? "true," : "false,");
	}

	@Override
	public void renderBoundColumnContentLink(String value, ContentLink link) {
		dataWidgetColumnInputWidget = link;
	}

	@Override
	public void renderFormContentLink(String value, ContentLink link) {
		preProcessFormItem(link, "bizContentLink");
		if (value != null) {
			code.append("value:'").append(OWASP.escapeJsString(value)).append("',");
		}
		disabled(link.getDisabledConditionName(), code);
		invisible(link.getInvisibleConditionName(), code);
		editable(link.getEditable(), code);
	}

	@Override
	public void renderFormContentSignature(ContentSignature signature) {
		// TODO not implemented for SC yet - use a ContentImage for now
		preProcessFormItem(signature, "bizContentImage");
		size(signature, null, code);
		disabled(signature.getDisabledConditionName(), code);
		invisible(signature.getInvisibleConditionName(), code);
		editable(Boolean.TRUE, code);
	}

	@Override
	public void renderBoundColumnHTML(HTML html) {
		dataWidgetColumnInputWidget = html;
	}

	@Override
	public void renderFormHTML(HTML html) {
		preProcessFormItem(html, "bizHTML");
		size(html, null, code);
		String mentionMarkers = html.getMentionMarkers();
		if (mentionMarkers != null) {
			code.append("mentionMarkers:'").append(OWASP.escapeJsString(mentionMarkers)).append("',");
		}
		disabled(html.getDisabledConditionName(), code);
		invisible(html.getInvisibleConditionName(), code);
	}

	@Override
	public void renderListMembership(String candidatesHeading, String membersHeading, ListMembership membership) {
		Relation relation = (Relation) getCurrentTarget().getAttribute();

		String variable = "v" + variableCounter++;
		code.append("var ").append(variable).append("=isc.BizListMembership.create({_b:'");
		code.append(BindUtil.sanitiseBinding(membership.getBinding()));
		code.append('\'');
		if (candidatesHeading != null) {
			code.append(",candidatesHeading:'");
			code.append(OWASP.escapeJsString(candidatesHeading)).append('\'');
		}
		if (membersHeading != null) {
			code.append(",membersHeading:'");
			code.append(OWASP.escapeJsString(membersHeading)).append('\'');
		}
		if ((relation instanceof Collection) && Boolean.TRUE.equals(((Collection) relation).getOrdered())) {
			code.append(",_ordinal:'").append(Bean.ORDINAL_NAME).append('\'');
		}
		code.append(",_view:view,");
		size(membership, DEFAULT_MIN_HEIGHT_IN_PIXELS, code);
		disabled(membership.getDisabledConditionName(), code);
		invisible(membership.getInvisibleConditionName(), code);

		containerVariables.push(variable);
	}

	@Override
	public void renderedListMembership(String candidatesHeading, String membersHeading, ListMembership membership) {
		removeTrailingComma(code);
		code.append("});\n");

		String variable = containerVariables.pop();
		code.append(containerVariables.peek()).append(".addContained(").append(variable).append(");\n");
	}

	@Override
	public void renderComparison(Comparison comparison) {
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
	public void renderBoundColumnLookupDescription(MetaDataQueryDefinition query,
													boolean canCreate,
													boolean canUpdate,
													String descriptionBinding,
													LookupDescription lookup) {
		dataWidgetColumnInputWidget = lookup;
	}

	@Override
	public void renderFormLookupDescription(MetaDataQueryDefinition query,
												boolean canCreate,
												boolean canUpdate,
												String descriptionBinding,
												LookupDescription lookup) {
		SmartClientFieldDefinition def = preProcessFormItem(lookup, "bizLookupDescription");
		size(lookup, null, code);
		disabled(lookup.getDisabledConditionName(), code);
		invisible(lookup.getInvisibleConditionName(), code);
		editable(lookup.getEditable(), code);
		disableLookupComponents(lookup, code);
		code.append("canCreate:").append(def.getLookup().getCanCreate());
		code.append(",canUpdate:").append(def.getLookup().getCanUpdate());

		code.append(",_view:view,");
		appendFilterParameters(lookup.getFilterParameters(), lookup.getParameters(), code);

		StringBuilder ds = new StringBuilder(256);
		String optionDataSource = def.getLookup().getOptionDataSource();
		SmartClientViewRenderer.appendDataSourceDefinition(user,
															customer,
															query,
															optionDataSource,
															lookup,
															currentUxUi,
															false,
															ds,
															new TreeSet<>());
		code.insert(0, ds);
	}

	@Override
	public void renderedBoundColumnLookupDescription(MetaDataQueryDefinition query,
														boolean canCreate,
														boolean canUpdate,
														String descriptionBinding,
														LookupDescription lookup) {
		// do nothing
	}

	@Override
	public void renderedFormLookupDescription(MetaDataQueryDefinition query,
												boolean canCreate,
												boolean canUpdate,
												String descriptionBinding,
												LookupDescription lookup) {
		// do nothing
	}

	@Override
	public void renderBoundColumnPassword(Password password) {
		dataWidgetColumnInputWidget = password;
	}

	@Override
	public void renderFormPassword(Password password) {
		preProcessFormItem(password, "password");
		size(password, null, code);
		// Security settings
		code.append("autoComplete:'none',browserAutoCapitalize:false,browserAutoCorrect:false,browserSpellCheck:false,");
		disabled(password.getDisabledConditionName(), code);
		invisible(password.getInvisibleConditionName(), code);
	}

	@Override
	public void renderedBoundColumnPassword(Password password) {
		// do nothing
	}

	@Override
	public void renderedFormPassword(Password password) {
		// do nothing
	}

	@Override
	public void renderBoundColumnRadio(Radio radio) {
		dataWidgetColumnInputWidget = radio;
	}

	@Override
	public void renderFormRadio(Radio radio) {
		preProcessFormItem(radio, "radioGroup");
		size(radio, null, code);
		if (Boolean.FALSE.equals(radio.getVertical())) {
			code.append("vertical:false,");
		}
		disabled(radio.getDisabledConditionName(), code);
		invisible(radio.getInvisibleConditionName(), code);
	}

	@Override
	public void renderedBoundColumnRadio(Radio radio) {
		// do nothing
	}

	@Override
	public void renderedFormRadio(Radio radio) {
		// do nothing
	}

	@Override
	public void renderBoundColumnRichText(RichText text) {
		dataWidgetColumnInputWidget = text;
	}

	@Override
	public void renderFormRichText(RichText text) {
		preProcessFormItem(text, "richText");
		size(text, DEFAULT_MIN_HEIGHT_IN_PIXELS, code);
		disabled(text.getDisabledConditionName(), code);
		invisible(text.getInvisibleConditionName(), code);
	}

	@Override
	public void renderedBoundColumnRichText(RichText richText) {
		// do nothing
	}

	@Override
	public void renderedFormRichText(RichText richText) {
		// do nothing
	}

	@Override
	public void renderBoundColumnSlider(Slider slider) {
		dataWidgetColumnInputWidget = slider;
	}

	@Override
	public void renderFormSlider(Slider slider) {
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
			code.append("numValues:").append(numberOfDiscreteValues.intValue() + 1).append(',');
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
	public void renderedBoundColumnSlider(Slider slider) {
		// do nothing
	}

	@Override
	public void renderedFormSlider(Slider slider) {
		// do nothing
	}

	@Override
	public void renderBoundColumnSpinner(Spinner spinner) {
		dataWidgetColumnInputWidget = spinner;
	}

	@Override
	public void renderFormSpinner(Spinner spinner) {
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
	public void renderedBoundColumnSpinner(Spinner spinner) {
		// do nothing
	}

	@Override
	public void renderedFormSpinner(Spinner spinner) {
		// do nothing
	}

	@Override
	public void renderBoundColumnTextArea(TextArea text) {
		dataWidgetColumnInputWidget = text;
	}

	@Override
	public void renderFormTextArea(TextArea text) {
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
	public void renderedBoundColumnTextArea(TextArea text) {
		// do nothing
	}

	@Override
	public void renderedFormTextArea(TextArea text) {
		// do nothing
	}

	@Override
	public void renderBoundColumnTextField(TextField text) {
		dataWidgetColumnInputWidget = text;
	}

	@Override
	public void renderFormTextField(TextField text) {
		CompleteType complete = text.getComplete();
		if (complete != null) {
			preProcessFormItem(text, "bizComplete");
			code.append(AbstractWebContext.ACTION_NAME).append(":'").append(complete).append("',");
			code.append(AbstractWebContext.BINDING_NAME).append(":'").append(BindUtil.sanitiseBinding(text.getBinding())).append("',");
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
	public void renderedBoundColumnTextField(TextField text) {
		// do nothing
	}

	@Override
	public void renderedFormTextField(TextField text) {
		// do nothing
	}
	
	@Override
	public void renderFormInject(Inject inject) {
		FormItem currentFormItem = getCurrentFormItem();
		if (currentFormItem != null) {
			// NB instead of preprocessFormItem(), handle title and required
			if (currentFormItem.getLabel() != null) {
				code.append("title:'").append(UtilImpl.processStringValue(getCurrentWidgetLabel())).append("',");
			}
			if (Boolean.TRUE.equals(currentFormItem.getRequired())) {
				code.append("required:true,");
			}
		}
		code.append(inject.getScript());
	}

	@Override
	public void renderInject(Inject inject) {
		code.append(inject.getScript());
	}

	@Override
	public void renderCustomAction(String name,
									String label,
									String iconUrl,
									String iconStyleClass,
									String toolTip,
									String confirmationText,
									ActionImpl action) {
		addAction(name, 
					null, 
					label,
					action.getInActionPanel(),
					action.getClientValidation(),
					iconUrl,
					iconStyleClass,
					toolTip,
					confirmationText,
					action.getParameters(),
					action.getDisabledConditionName(),
					action.getInvisibleConditionName(),
					null);
	}

	@Override
	public void renderAddAction(String name,
									String label,
									String iconUrl,
									String iconStyleClass,
									String toolTip,
									String confirmationText,
									ActionImpl action) {
		addAction(name,
					ImplicitActionName.Add,
					label,
					action.getInActionPanel(),
					action.getClientValidation(),
					iconUrl,
					iconStyleClass,
					toolTip,
					confirmationText,
					action.getParameters(),
					action.getDisabledConditionName(),
					action.getInvisibleConditionName(),
					null);
	}

	@Override
	public void renderRemoveAction(String name,
									String label,
									String iconUrl,
									String iconStyleClass,
									String toolTip,
									String confirmationText,
									ActionImpl action,
									boolean canDelete) {
		addAction(name,
					ImplicitActionName.Remove,
					label,
					action.getInActionPanel(),
					action.getClientValidation(),
					iconUrl,
					iconStyleClass,
					toolTip,
					confirmationText,
					action.getParameters(),
					action.getDisabledConditionName(),
					action.getInvisibleConditionName(),
					Boolean.valueOf(canDelete));
	}

	@Override
	public void renderZoomOutAction(String name,
										String label,
										String iconUrl,
										String iconStyleClass,
										String toolTip,
										String confirmationText,
										ActionImpl action) {
		addAction(name,
					ImplicitActionName.ZoomOut,
					label,
					action.getInActionPanel(),
					action.getClientValidation(),
					iconUrl,
					iconStyleClass,
					toolTip,
					confirmationText,
					action.getParameters(),
					action.getDisabledConditionName(),
					action.getInvisibleConditionName(),
					null);
	}

	@Override
	public void renderNavigateAction(String name,
										String label,
										String iconUrl,
										String iconStyleClass,
										String toolTip,
										String confirmationText,
										ActionImpl action) {
		addAction(name,
					ImplicitActionName.Navigate,
					label,
					action.getInActionPanel(),
					action.getClientValidation(),
					iconUrl,
					iconStyleClass,
					toolTip,
					confirmationText,
					action.getParameters(),
					action.getDisabledConditionName(),
					action.getInvisibleConditionName(),
					null);
	}

	@Override
	public void renderOKAction(String name,
								String label,
								String iconUrl,
								String iconStyleClass,
								String toolTip,
								String confirmationText,
								ActionImpl action) {
		addAction(name,
					ImplicitActionName.OK,
					label,
					action.getInActionPanel(),
					action.getClientValidation(),
					iconUrl,
					iconStyleClass,
					toolTip,
					confirmationText,
					action.getParameters(),
					action.getDisabledConditionName(),
					action.getInvisibleConditionName(),
					null);
	}

	@Override
	public void renderSaveAction(String name,
									String label,
									String iconUrl,
									String iconStyleClass,
									String toolTip,
									String confirmationText,
									ActionImpl action) {
		addAction(name,
					ImplicitActionName.Save,
					label,
					action.getInActionPanel(),
					action.getClientValidation(),
					iconUrl,
					iconStyleClass,
					toolTip,
					confirmationText,
					action.getParameters(),
					action.getDisabledConditionName(),
					action.getInvisibleConditionName(),
					null);
	}

	@Override
	public void renderCancelAction(String name,
									String label,
									String iconUrl,
									String iconStyleClass,
									String toolTip,
									String confirmationText,
									ActionImpl action) {
		addAction(name,
					ImplicitActionName.Cancel,
					label,
					action.getInActionPanel(),
					action.getClientValidation(),
					iconUrl,
					iconStyleClass,
					toolTip,
					confirmationText,
					action.getParameters(),
					action.getDisabledConditionName(),
					action.getInvisibleConditionName(),
					null);
	}

	@Override
	public void renderDeleteAction(String name,
									String label,
									String iconUrl,
									String iconStyleClass,
									String toolTip,
									String confirmationText,
									ActionImpl action) {
		addAction(name,
					ImplicitActionName.Delete,
					label,
					action.getInActionPanel(),
					action.getClientValidation(),
					iconUrl,
					iconStyleClass,
					toolTip,
					confirmationText,
					action.getParameters(),
					action.getDisabledConditionName(),
					action.getInvisibleConditionName(),
					null);
	}

	@Override
	public void renderReportAction(String name,
									String label,
									String iconUrl,
									String iconStyleClass,
									String toolTip,
									String confirmationText,
									ActionImpl action) {
		addAction(name,
					ImplicitActionName.Report,
					label,
					action.getInActionPanel(),
					action.getClientValidation(),
					iconUrl,
					iconStyleClass,
					toolTip,
					confirmationText,
					action.getParameters(),
					action.getDisabledConditionName(),
					action.getInvisibleConditionName(),
					null);
	}

	@Override
	public void renderBizExportAction(String name,
										String label,
										String iconUrl,
										String iconStyleClass,
										String toolTip,
										String confirmationText,
										ActionImpl action) {
		addAction(name,
					ImplicitActionName.BizExport,
					label,
					action.getInActionPanel(),
					action.getClientValidation(),
					iconUrl,
					iconStyleClass,
					toolTip,
					confirmationText,
					action.getParameters(),
					action.getDisabledConditionName(),
					action.getInvisibleConditionName(),
					null);
	}

	@Override
	public void renderBizImportAction(String name,
										String label,
										String iconUrl,
										String iconStyleClass,
										String toolTip,
										String confirmationText,
										ActionImpl action) {
		addAction(name,
					ImplicitActionName.BizImport,
					label,
					action.getInActionPanel(),
					action.getClientValidation(),
					iconUrl,
					iconStyleClass,
					toolTip,
					confirmationText,
					action.getParameters(),
					action.getDisabledConditionName(),
					action.getInvisibleConditionName(),
					null);
	}

	@Override
	public void renderDownloadAction(String name,
										String label,
										String iconUrl,
										String iconStyleClass,
										String toolTip,
										String confirmationText,
										ActionImpl action) {
		addAction(name,
					ImplicitActionName.Download,
					label,
					action.getInActionPanel(),
					action.getClientValidation(),
					iconUrl,
					iconStyleClass,
					toolTip,
					confirmationText,
					action.getParameters(),
					action.getDisabledConditionName(),
					action.getInvisibleConditionName(),
					null);
	}

	@Override
	public void renderUploadAction(String name,
									String label,
									String iconUrl,
									String iconStyleClass,
									String toolTip,
									String confirmationText,
									ActionImpl action) {
		addAction(name,
					ImplicitActionName.Upload,
					label,
					action.getInActionPanel(),
					action.getClientValidation(),
					iconUrl,
					iconStyleClass,
					toolTip,
					confirmationText,
					action.getParameters(),
					action.getDisabledConditionName(),
					action.getInvisibleConditionName(),
					null);
	}

	@Override
	public void renderNewAction(String name,
									String label,
									String iconUrl,
									String iconStyleClass,
									String toolTip,
									String confirmationText,
									ActionImpl action) {
		addAction(name,
					ImplicitActionName.New,
					label,
					action.getInActionPanel(),
					action.getClientValidation(),
					iconUrl,
					iconStyleClass,
					toolTip,
					confirmationText,
					action.getParameters(),
					action.getDisabledConditionName(),
					action.getInvisibleConditionName(),
					null);
	}

	@Override
	public void renderEditAction(String name,
									String label,
									String iconUrl,
									String iconStyleClass,
									String toolTip,
									String confirmationText,
									ActionImpl action) {
		addAction(name,
					ImplicitActionName.Edit,
					label,
					action.getInActionPanel(),
					action.getClientValidation(),
					iconUrl,
					iconStyleClass,
					toolTip,
					confirmationText,
					action.getParameters(),
					action.getDisabledConditionName(),
					action.getInvisibleConditionName(),
					null);
	}

	@Override
	public void renderPrintAction(String name,
									String label,
									String iconUrl,
									String iconStyleClass,
									String toolTip,
									String confirmationText,
									ActionImpl action) {
		addAction(name,
					ImplicitActionName.Print,
					label,
					action.getInActionPanel(),
					action.getClientValidation(),
					iconUrl,
					iconStyleClass,
					toolTip,
					confirmationText,
					action.getParameters(),
					action.getDisabledConditionName(),
					action.getInvisibleConditionName(),
					null);
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
	public void visitServerSideActionEventAction(Action action, ServerSideActionEventAction server) {
		if (getCurrentForm() != null) {
			writeOutServerSideCallbackMethodIfNecessary();
		}
		code.append(visitingAsync ? "view.doBlurryAction('" : "view.doAction('");
		String name = action.getResourceName();
		ImplicitActionName implicitName = action.getImplicitName();
		if (implicitName != null) {
			name = implicitName.toString();
		}
		code.append(name).append("',");
		code.append(! Boolean.FALSE.equals(action.getClientValidation())).append(");");
	}

	@Override
	public void visitOnChangedEventHandler(Changeable changeable, boolean parentVisible, boolean parentEnabled) {
		if (getCurrentForm() == null) {
			code.append("changed:function(){var view=this._view;");
		}
		else {
			code.append("changed:function(form,item,value){var view=form._view;");
		}
	}

	@Override
	public void visitedOnChangedEventHandler(Changeable changeable, boolean parentVisible, boolean parentEnabled) {
		code.append("},");
	}

	@Override
	public void visitOnFocusEventHandler(Focusable blurable, boolean parentVisible, boolean parentEnabled) {
		// Note the test to short circuit focus event processing whilst requests are pending to stop loops with multiple fields.
		code.append("editorEnter:function(form,item,value){if((!isc.RPCManager.requestsArePending())&&item.validate()){var view=form._view;");
	}

	@Override
	public void visitedOnFocusEventHandler(Focusable blurable, boolean parentVisible, boolean parentEnabled) {
		code.append("}},");
	}

	// indicates that we are blurring or selecting and we need to call special methods
	// to potentially serialize calls to button actions after editorExit.
	private boolean visitingAsync = false;
	
	@Override
	public void visitOnBlurEventHandler(Focusable blurable, boolean parentVisible, boolean parentEnabled) {
		visitingAsync = true;
		
		// This fires before the BizButton action() method if a button was clicked
		// Note the test to short circuit blur event processing whilst requests are pending to stop loops with multiple fields.
		code.append("blur:function(form,item){if(isc.RPCManager.requestsArePending()){form._view._blurry=null;}else{form._view._blurry=item;}},");
		// This is called before or after the BizButton action depending on the browser.
		// Note the test to short circuit blur event processing whilst requests are pending to stop loops with multiple fields.
		code.append("editorExit:function(form,item,value){if(isc.RPCManager.requestsArePending()||(!item.validate())){form._view._blurry=null;}else{var view=form._view;");
	}

	@Override
	public void visitedOnBlurEventHandler(Focusable blurable, boolean parentVisible, boolean parentEnabled) {
		code.append("}},");
		visitingAsync = false;
	}

	// Used to sort out server-side events into the bizEditedForServer() method.
	private boolean inOnAddedEventHandler = false;

	@Override
	public void visitOnAddedEventHandler(Addable addable, boolean parentVisible, boolean parentEnabled) {
		if (dataWidgetFieldsIncomplete) {
			code.setLength(code.length() - 1);
			code.append("],");
			dataWidgetFieldsIncomplete = false;
		}
		inOnAddedEventHandler = true;
		if (getCurrentForm() == null) {
			code.append("bizAdded:function(){var view=this._view;");
		}
		else {
			code.append("bizAdded:function(form,item,value){var view=form._view;");
		}
	}

	@Override
	public void visitedOnAddedEventHandler(Addable addable, boolean parentVisible, boolean parentEnabled) {
		code.append("},");
		inOnAddedEventHandler = false;
	}

	// Used to sort out server-side events into the bizEditedForServer() method.
	private boolean inOnEditedEventHandler = false;

	@Override
	public void visitOnEditedEventHandler(Editable editable, boolean parentVisible, boolean parentEnabled) {
		if (dataWidgetFieldsIncomplete) {
			code.setLength(code.length() - 1);
			code.append("],");
			dataWidgetFieldsIncomplete = false;
		}
		inOnEditedEventHandler = true;
		if (getCurrentForm() == null) {
			code.append("bizEdited:function(){var view=this._view;");
		}
		else {
			code.append("bizEdited:function(form,item,value){var view=form._view;");
		}
	}

	@Override
	public void visitedOnEditedEventHandler(Editable editable, boolean parentVisible, boolean parentEnabled) {
		code.append("},");
		inOnEditedEventHandler = false;
	}

	// Used to sort out server-side events into the bizEditedForServer() method.
	private boolean inOnRemovedEventHandler = false;

	@Override
	public void visitOnRemovedEventHandler(Removable removable, boolean parentVisible, boolean parentEnabled) {
		if (dataWidgetFieldsIncomplete) {
			code.setLength(code.length() - 1);
			code.append("],");
			dataWidgetFieldsIncomplete = false;
		}
		inOnRemovedEventHandler = true;
		if (getCurrentForm() == null) {
			code.append("bizRemoved:function(){var view=this._view;");
		}
		else {
			code.append("bizRemoved:function(form,item,value){var view=form._view;");
		}
	}

	@Override
	public void visitedOnRemovedEventHandler(Removable removable, boolean parentVisible, boolean parentEnabled) {
		code.append("},");
		inOnRemovedEventHandler = false;
	}

	@Override
	public void visitOnSelectedEventHandler(Selectable selectable, boolean parentVisible, boolean parentEnabled) {
		if ((dataWidgetVariable != null) || (listWidgetVariable != null)) {
			visitingAsync = true;
		}
		if (dataWidgetFieldsIncomplete) {
			code.setLength(code.length() - 1);
			code.append("],");
			dataWidgetFieldsIncomplete = false;
		}
		code.append("bizSelected:function(){var view=this._view;");
	}

	@Override
	public void visitedOnSelectedEventHandler(Selectable selectable, boolean parentVisible, boolean parentEnabled) {
		code.append("},");
		visitingAsync = false;
	}

	@Override
	public void visitOnPickedEventHandler(LookupDescription lookup, boolean parentVisible, boolean parentEnabled) {
		code.append("bizPicked:function(form,item,value){var view=form._view;");
	}

	@Override
	public void visitedOnPickedEventHandler(LookupDescription lookup, boolean parentVisible, boolean parentEnabled) {
		code.append("},");
	}

	@Override
	public void visitOnClearedEventHandler(LookupDescription lookup, boolean parentVisible, boolean parentEnabled) {
		code.append("bizCleared:function(form,item,value){var view=form._view;");
	}

	@Override
	public void visitedOnClearedEventHandler(LookupDescription lookup, boolean parentVisible, boolean parentEnabled) {
		code.append("},");
	}

	@Override
	public void visitRerenderEventAction(RerenderEventAction rerender,
											EventSource source,
											boolean parentVisible,
											boolean parentEnabled) {
		if (getCurrentForm() != null) {
			writeOutServerSideCallbackMethodIfNecessary();
		}
		code.append(visitingAsync ? "view.rerenderBlurryAction(" : "view.rerenderAction(");
		code.append(Boolean.FALSE.equals(rerender.getClientValidation()) ? "false,'" : "true,'");
		code.append(source.getSource()).append("');");
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

	@Override
	public void visitParameter(Parameter parameter, boolean parentVisible, boolean parentEnabled) {
		// do nothing - parameters are handled separately
	}

	@Override
	public void visitFilterParameter(FilterParameter parameter, boolean parentVisible, boolean parentEnabled) {
		// do nothing - parameters are handled separately
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
				if (sizable instanceof RelativeWidth) {
					RelativeWidth relative = (RelativeWidth) sizable;
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
					(getCurrentFormItem() != null) && 
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

	private static void disableLookupComponents(LookupDescription lookup, StringBuilder builder) {
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

	private static void bordered(String title, Bordered bordered, Integer definedPixelPadding, StringBuilder builder) {
		if (Boolean.TRUE.equals(bordered.getBorder())) {
			border(title, definedPixelPadding, builder);
		}
	}

	private static void border(String title, Integer definedPixelPadding, StringBuilder builder) {
		builder.append("styleName:'bizhubRoundedBorder',groupBorderCSS:'1px solid #bfbfbf',isGroup:true,margin:1,groupLabelBackgroundColor:'transparent',");
		if (title != null) {
			builder.append("groupTitle:'&nbsp;&nbsp;").append(OWASP.escapeJsString(title));
			builder.append("&nbsp;&nbsp;',groupLabelStyleName:'bizhubBorderLabel',");
		}
		if (definedPixelPadding == null) {
			builder.append("layoutMargin:10,");
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

	private void addAction(String actionName,
							ImplicitActionName implicitName,
							String displayName,
							Boolean inActionPanel,
							Boolean clientValidation,
							String iconUrl,
							String iconStyleClass,
							String tooltip,
							String confirmationText,
							List<Parameter> parameters,
							String disabledConditionName,
							String invisibleConditionName,
							Boolean canDelete) { // null unless its a remove button
		if (! Boolean.FALSE.equals(inActionPanel) && 
				(! ImplicitActionName.Add.equals(implicitName)) &&
				(! ImplicitActionName.Edit.equals(implicitName))) {
			String buttonCode = generateButton(actionName,
												implicitName,
												displayName,
												clientValidation,
												iconUrl,
												iconStyleClass,
												tooltip,
												confirmationText,
												parameters,
												disabledConditionName,
												invisibleConditionName,
												null,
												canDelete);
			// use double quote string delimiter to allow &quot; HTML character entity
			code.append("view.add");
			if (! noCreateView) {
				code.append(ViewType.edit.toString().equals(view.getName()) ? "Edit" : "Create");
			}
			code.append("Action(");
			code.append(buttonCode).append(");");
		}
	}

	private String generateZoomIn(String label,
									String iconUrl,
									String iconStyleClass,
									String toolTip,
									ZoomIn zoomIn) {
		StringBuilder result = new StringBuilder(128);
		result.append("isc.BizZoomIn.create({binding:'");
		result.append(BindUtil.sanitiseBinding(zoomIn.getBinding()));
		if ((label != null) || (iconStyleClass != null)) {
			result.append("',displayName:'");
			if (iconStyleClass != null) {
				result.append("<i class=\"bizhubFontIcon ").append(iconStyleClass).append("\"></i>");
			}
			if (label != null) {
				result.append("<span> &nbsp;</span>").append(OWASP.escapeJsString(label));
			}
		}
		result.append("',tabIndex:999,");
		if ((iconStyleClass == null) && (iconUrl != null)) {
			result.append("icon:'../").append(iconUrl).append("',");
		}
		size(zoomIn, null, result);
		disabled(zoomIn.getDisabledConditionName(), result);
		invisible(zoomIn.getInvisibleConditionName(), result);
		if (toolTip != null) {
			result.append("tooltip:'").append(OWASP.escapeJsString(toolTip)).append("',");
		}
		result.append("_view:view})");
		
		return result.toString();
	}

	private String generateButton(String actionName,
									ImplicitActionName implicitName,
									String label,
									Boolean clientValidation,
									String iconUrl,
									String iconStyleClass,
									String toolTip,
									String confirmationText,
									List<Parameter> parameters,
									String disabledConditionName,
									String invisibleConditionName,
									Button button, // null if called from an action defn
									Boolean canDelete) { // null for anything but remove button
		StringBuilder result = new StringBuilder(128);
		result.append("isc.BizButton.create({validate:");
		result.append(! Boolean.FALSE.equals(clientValidation));
		result.append(",type:");
		if (implicitName == null) {
			result.append("null");
		}
		else {
			result.append('\'').append(implicitName).append('\'');
		}
		result.append(",actionName:'").append(actionName);
		if ((label != null) || (iconStyleClass != null)) {
			result.append("',displayName:'");
			if (iconStyleClass != null) {
				result.append("<i class=\"bizhubFontIcon ").append(iconStyleClass).append("\"></i>");
			}
			if (label != null) {
				result.append("<span> &nbsp;</span>").append(OWASP.escapeJsString(label));
			}
		}
		result.append("',tabIndex:999,");
		if ((iconStyleClass == null) && (iconUrl != null)) {
			result.append("icon:'../").append(iconUrl).append("',");
		}
		if (button != null) {
			size(button, null, result);
		}
		disabled(disabledConditionName, result);
		invisible(invisibleConditionName, result);
		if (toolTip != null) {
			result.append("tooltip:'").append(OWASP.escapeJsString(toolTip)).append("',");
		}
		if (confirmationText != null) {
			result.append("confirm:'").append(OWASP.escapeJsString(confirmationText)).append("',");
		}
		appendParameters(parameters, result);
		if (canDelete != null) {
			result.append("_canDelete:").append(canDelete).append(',');
		}
		result.append("_view:view})");
		
		return result.toString();
	}
	
	private static void appendParameters(List<Parameter> parameters, StringBuilder builder) {
		if ((parameters != null) && (! parameters.isEmpty())) {
			builder.append("params:{");
			for (Parameter parameter : parameters) {
				builder.append("'").append(BindUtil.sanitiseBinding(parameter.getName())).append("':");
				String binding = parameter.getValueBinding();
				if (binding != null) {
					builder.append("'{").append(binding).append("}',");
				}
				else {
					String value = parameter.getValue();
					if (value == null) {
						builder.append("null,");
					}
					else {
						builder.append("'").append(value).append("',");
					}
				}
			}
			builder.setLength(builder.length() - 1); // remove comma
			builder.append("},");
		}
	}

	private static void appendFilterParameters(List<FilterParameter> filterParameters,
												List<Parameter> parameters,
												StringBuilder builder) {
		if (((filterParameters != null) && (! filterParameters.isEmpty())) ||
				((parameters != null) && (! parameters.isEmpty()))) {
			builder.append("params:[");
			if (filterParameters != null) {
				for (FilterParameter parameter : filterParameters) {
					builder.append("{name:'").append(BindUtil.sanitiseBinding(parameter.getFilterBinding())).append("',operator:'");
					builder.append(SmartClientFilterOperator.fromFilterOperator(parameter.getOperator())).append("',value:");
					String binding = parameter.getValueBinding();
					if (binding != null) {
						builder.append("'{").append(binding).append("}'},");
					}
					else {
						String value = parameter.getValue();
						if (value == null) {
							builder.append("null},");
						}
						else {
							builder.append("'").append(value).append("'},");
						}
					}
				}
			}
			if (parameters != null) {
				for (Parameter parameter : parameters) {
					builder.append("{name:':").append(BindUtil.sanitiseBinding(parameter.getName()));
					builder.append("',operator:'").append(SmartClientFilterOperator.equals).append("',value:");
					String binding = parameter.getValueBinding();
					if (binding != null) {
						builder.append("'{").append(binding).append("}'},");
					}
					else {
						String value = parameter.getValue();
						if (value == null) {
							builder.append("null},");
						}
						else {
							builder.append("'").append(value).append("'},");
						}
					}
				}
			}			
			builder.setLength(builder.length() - 1); // remove comma
			builder.append("],");
		}
	}
	
	private SmartClientFieldDefinition preProcessFormItem(InputWidget widget,
															String typeOverride) {
		SmartClientFieldDefinition def = getField(document, widget, true);
		if (typeOverride != null) {
			def.setType(typeOverride);
		}
		String title = getCurrentWidgetLabel();
		if (title != null) {
			
			def.setTitle(title);
		}
		def.setRequired(isCurrentWidgetRequired());
		String help = getCurrentWidgetHelp();
		if (help != null) {
			def.setHelpText(help);
		}
		
		code.append(def.toJavascript());
		code.append(',');

		return def;
	}
	
	public static SmartClientQueryColumnDefinition getQueryColumn(User user,
																	Customer customer,
																	Module module,
																	Document document,
																	MetaDataQueryColumn column,
																	boolean runtime,
																	String uxui) {
		return new SmartClientQueryColumnDefinition(user, customer, module, document, column, runtime, uxui);
	}

	/**
	 * Get the smart client field definition given the widget/binding.
	 * If bindingOverride is defined, it will be used to determine the field to use.
	 * bindingOverride is used when a Datagrid has a lookupDescription which has no binding.
	 * That is, the lookupDescription is for the entire dataGrid entity.
	 * 
	 * @param document
	 * @param widget	The widget metadata to use to define the smart client form field
	 * @param bindingOverride	If defined, specifies a different binding to use.
	 * @return
	 */
	public SmartClientFieldDefinition getField(@SuppressWarnings("hiding") Document document, 
												InputWidget widget,
												boolean runtime) {
		return new SmartClientFieldDefinition(user, customer, module, document, widget, runtime, currentUxUi);
	}
	
	public SmartClientDataGridFieldDefinition getDataGridField(@SuppressWarnings("hiding") Document document,
    															InputWidget widget,
    															String dataGridBinding,
    															boolean hasFormatter,
    															boolean runtime) {
    	return new SmartClientDataGridFieldDefinition(user, customer, module, document, widget, dataGridBinding, hasFormatter, runtime, currentUxUi);
    }

    /**
     * Appends a data source definition from a document list model.
     * @param user
     * @param customer
     * @param owningModule
     * @param owningDocument
     * @param modelName
     * @param config	Whether to create a partial config data source defn for the menu items
     * @param toAppendTo	definition is appended to this
     * @param visitedQueryNames
     * @return	The ID for the query definition generated.
     */
	public static String appendDataSourceDefinition(User user,
														Customer customer,
														Module owningModule,
														Document owningDocument,
														String modelName,
														String uxui,
														boolean config,
														StringBuilder toAppendTo,
														Set<String> visitedQueryNames) {
		ListModel<Bean> model = owningDocument.getListModel(customer, modelName, true);
		// Note we cannot set the bean on the model here as we are only generating out the UI.
		Document drivingDocument = model.getDrivingDocument();
		if (drivingDocument == null) {
			throw new MetaDataException("List Model" + model + " has no driving document defined and smart client does not support dynamic/late list grid generation");
		}
		Module drivingDocumentModule = customer.getModule(drivingDocument.getOwningModuleName());

		return appendDataSourceDefinition(user, 
											customer,
											owningModule.getName(),
											owningDocument,
											drivingDocumentModule,
											drivingDocument,
											null,
											false,
											modelName,
											model.getLocalisedDescription(),
											model.getColumns(),
											null, 
											null, 
											uxui,
											config, 
											toAppendTo, 
											visitedQueryNames);
	}
	
	/**
     * Appends a data source definition from a module query.
     * @param customer
     * @param query
     * @param dataSourceIDOverride	ID of created data source if mandated
     * @param hiddenBindings	Extra bindings to include in the data source - not mandatory
     * @param config	Whether to create a partial config data source defn for the menu items
     * @param toAppendTo	definition is appended to this
     * @param visitedQueryNames
     * @return	The ID for the query definition generated.
     */
	public static String appendDataSourceDefinition(User user,
														Customer customer,
														MetaDataQueryDefinition query,
														String dataSourceIDOverride,
														LookupDescription forLookup,
														String uxui,
														boolean config,
														StringBuilder toAppendTo,
														Set<String> visitedQueryNames) {
		String documentName = query.getDocumentName();
		Module documentModule = query.getDocumentModule(customer);
		Module owningModule = query.getOwningModule();
		Document drivingDocument = documentModule.getDocument(customer, documentName);
		return appendDataSourceDefinition(user, 
											customer, 
											owningModule.getName(), 
											drivingDocument,
											documentModule, 
											drivingDocument, 
											query.getName(), 
											query.isAggregate(),
											null, 
											query.getLocalisedDescription(),
											query.getColumns(),
											dataSourceIDOverride, 
											forLookup, 
											uxui,
											config, 
											toAppendTo, 
											visitedQueryNames);
	}
	
	@SuppressWarnings("null")
	private static String appendDataSourceDefinition(User user,
														Customer customer,
														String owningModuleName,
														Document owningDocument,
														Module drivingDocumentModule,
														Document drivingDocument,
														String queryName,
														boolean aggregateQuery,
														String modelName,
														String description,
														List<MetaDataQueryColumn> columns,
														String dataSourceIDOverride,
														LookupDescription forLookup,
														String uxui,
														// indicates that this is for configuration in the harness page
														boolean config,
														StringBuilder toAppendTo,
														@Nonnull Set<String> visitedQueryNames) {
		// dataSourceId -> defn
		Map<String, String> childDataSources = new TreeMap<>();
		
		String drivingDocumentName = drivingDocument.getName();
		String dataSourceId = null;
		if (dataSourceIDOverride != null) {
			dataSourceId = dataSourceIDOverride;
		}
		else if (queryName != null) {
			dataSourceId = new StringBuilder(32).append(owningModuleName).append('_').append(queryName).toString();
		}
		else if (modelName != null) {
			// NB 4 tokens, not 3
			dataSourceId = new StringBuilder(32).append(owningModuleName).append('_').append(owningDocument.getName()).append("__").append(modelName).toString();
		}

		// Short circuit duplicate recursive data source column traversals
		if (visitedQueryNames.contains(dataSourceId)) {
			return dataSourceId;
		}
		// NB Add the visited query name here before processing the query columns in case 1 of the query columns is a reference using the same query/datasource
		visitedQueryNames.add(dataSourceId);

		if (config) {
			toAppendTo.append('{');
		}
		else {
			toAppendTo.append("if(window.").append(dataSourceId);
			toAppendTo.append("){}else{isc.RestDataSource.create({dataFormat:'json',jsonPrefix:'',jsonSuffix:'',dataURL:'smartlist',defaultTextMatchStyle:'substring',");
			toAppendTo.append("operationBindings:[{operationType:'fetch',dataProtocol:'postParams'},");
			toAppendTo.append("{operationType:'update',dataProtocol:'postParams'},");
			toAppendTo.append("{operationType:'add',dataProtocol:'postParams'},");
			toAppendTo.append("{operationType:'remove',dataProtocol:'postParams'}],");
		}
		toAppendTo.append("ID:'").append(dataSourceId);
		toAppendTo.append("',modoc:'");
		toAppendTo.append(drivingDocumentModule.getName());
		toAppendTo.append('.');
		toAppendTo.append(drivingDocumentName);

		String icon = drivingDocument.getIconStyleClass();
		if (icon != null) {
			toAppendTo.append("',fontIcon:'").append(icon);
		}
		else {
			String icon32 = drivingDocument.getIcon32x32RelativeFileName();
			if (icon32 != null) {
				toAppendTo.append("',icon:'").append(icon32);
			}
		}

		if (! config) {
			// ensure all filtering is server-side
			// this enables the summary row to always stay in sync and
			// lookups to drop down with the same criteria but load from the server
			// NB _drop is set to true in bizLookupDescription.showPicker() JS.
			toAppendTo.append("',compareCriteria:function(newCriteria,oldCriteria,requestProperties,policy){if(this._drop){return -1;}else{return this.Super('compareCriteria',arguments)}}");
			toAppendTo.append(",_drop:false");
			toAppendTo.append(",transformResponse:function(dsResponse,dsRequest,data){this._drop=false;return this.Super('transformResponse',arguments)}");
			toAppendTo.append(",criteriaPolicy:'dropOnChange");
		}
		toAppendTo.append("',aggregate:").append(aggregateQuery);
		toAppendTo.append(",canCreate:").append(user.canCreateDocument(drivingDocument));
		toAppendTo.append(",canUpdate:").append(user.canUpdateDocument(drivingDocument));
		toAppendTo.append(",canDelete:").append(user.canDeleteDocument(drivingDocument));
		toAppendTo.append(",title:'");
		toAppendTo.append(OWASP.escapeJsString(description));
		toAppendTo.append("',fields:[");

		if (! config) {
			toAppendTo.append("{name:'bizTagged',title:'");
			toAppendTo.append(OWASP.escapeJsString(Util.i18n("ui.tag"), false, true));
			toAppendTo.append("',type:'boolean',validOperators:['equals']},");
			toAppendTo.append("{name:'bizFlagComment',title:'");
			toAppendTo.append(OWASP.escapeJsString(Util.i18n("ui.flag"), false, true));
			toAppendTo.append("'},"); //,length:1024} long length makes filter builder use a text area
		}
		
		if (drivingDocumentName.equals(drivingDocument.getParentDocumentName())) { // hierarchical
			toAppendTo.append("{name:'bizParentId',title:'Parent ID',type:'text',hidden:true,foreignKey:'");
			toAppendTo.append(dataSourceId).append(".bizId'},");
		}
		
		List<String> hiddenBindingsList = new ArrayList<>();
		if (forLookup != null) {
			hiddenBindingsList.add(forLookup.getDescriptionBinding());
			List<FilterParameter> filterParameters = forLookup.getFilterParameters();
			if (filterParameters != null) {
				for (FilterParameter parameter : filterParameters) {
					hiddenBindingsList.add(parameter.getFilterBinding());
				}
			}
			List<Parameter> parameters = forLookup.getParameters();
			if (parameters != null) {
				for (Parameter parameter : parameters) {
					hiddenBindingsList.add(parameter.getName());
				}
			}
		}
		
		int cellHeight = 0; // fixed cell height of list grid (defined in data source)
		
		for (MetaDataQueryColumn column : columns) {
			if ((column instanceof MetaDataQueryProjectedColumn) && 
					(! ((MetaDataQueryProjectedColumn) column).isProjected())) {
				continue;
			}

			SmartClientQueryColumnDefinition def = getQueryColumn(user, customer, drivingDocumentModule, drivingDocument, column, true, uxui);
			if (def.isHasDisplayField()) {
				hiddenBindingsList.add("_display_" + def.getName());
			}
			toAppendTo.append('{').append(def.toJavascript()).append("},");

			// define the minimum fixed cell height for the grid (dataSource) based on any content image columns
			Integer pixelHeight = def.getPixelHeight();
			if (pixelHeight != null) {
				int h = pixelHeight.intValue();
				if (h > cellHeight) {
					cellHeight = h;
				}
			}

			SmartClientLookupDefinition lookup = def.getLookup();
			if (lookup != null) {
				// Add lookup description data source field
				toAppendTo.append("{name:'");
				boolean bindingToDataGrid = lookup.isBindingToDataGrid();
	        	if (! bindingToDataGrid) {
	        		toAppendTo.append(def.getName()).append('_');
	        	}
	        	toAppendTo.append(lookup.getDisplayField()).append("',type:'text',hidden:'true'},");

				StringBuilder childDataSourceDefinition = new StringBuilder(512);
				String childDataSourceId = appendDataSourceDefinition(user,
																		customer,
																		lookup.getQuery(),
																		lookup.getOptionDataSource(),
																		null,
																		uxui,
																		config,
																		childDataSourceDefinition,
																		visitedQueryNames);
				childDataSources.put(childDataSourceId, childDataSourceDefinition.toString());
			}
			
			if (hiddenBindingsList != null) {
				hiddenBindingsList.remove(column.getBinding());
			}
		}
		
		if (! config) {
			// for filtering
			toAppendTo.append("{name: 'operator', type: 'text', hidden: true},");
			toAppendTo.append("{name: 'criteria', type: 'text', hidden: true},");

			// standard for all rows
			toAppendTo.append("{name: 'bizId', primaryKey: true, hidden: true},");
			toAppendTo.append("{name:'bizLock', hidden: true},");
		}
		
		for (String hiddenBinding : hiddenBindingsList) {
			toAppendTo.append("{name:'").append(BindUtil.sanitiseBinding(hiddenBinding));
			toAppendTo.append("',type:'text',hidden:true},");
		}

		if (toAppendTo.charAt(toAppendTo.length() - 1) == ',') { // if we have a comma then we at least have a field in the data source
			toAppendTo.setLength(toAppendTo.length() - 1); // remove the last field comma
		}
		toAppendTo.append("]");
		
		// Add cellHeight if applicable
		if (cellHeight > 0) {
			toAppendTo.append(",cellHeight:").append(cellHeight);
		}
		
		if (config) {
			toAppendTo.append("},\n");
		}
		else {
			toAppendTo.append("});}\n");
		}
		
		// Add any child datasources found
		for (String childDataSourceDefinition : childDataSources.values()) {
			toAppendTo.append(childDataSourceDefinition).append('\n');
		}
		
		return dataSourceId;
	}
	
	private static void validateCollapsible(Collapsible collapsible,String borderTitle) {
		if (collapsible != null && borderTitle == null) {
			throw new MetaDataException("Border title must be defined if the collapsible attribute is present");
		}
	}

	@Override
	public void renderSidebar(Sidebar sidebar) {
		String variable = "v" + variableCounter++;
		code.append("var ").append(variable).append("=isc.BizVBox.create({width:'100%',height:'100%',");
		invisible(sidebar.getInvisibleConditionName(), code);
		removeTrailingComma(code);
		code.append("});\n");
		
		code.append("sidebarPane.addContained(").append(variable).append(");\n");
		containerVariables.push(variable);
	}
	
	@Override
	public void renderedSidebar(Sidebar sidebar) {
		containerVariables.pop();
	}
}
