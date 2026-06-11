package org.skyve.impl.web.service.smartclient;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.List;
import java.util.Map;
import java.util.Set;
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
import jakarta.annotation.Nullable;

/**
 * Renders Skyve metadata into SC Skyve Javascript.
 */
@SuppressWarnings("java:S1192") // Repeated literals are deliberate SmartClient JSON/script output fragments.
public class SmartClientViewRenderer extends ViewRenderer {
	private static final Integer DEFAULT_MIN_HEIGHT_IN_PIXELS = Integer.valueOf(170);
	private static final Integer DEFAULT_TAB_MIN_HEIGHT_IN_PIXELS = Integer.valueOf(200);

	private boolean noCreateView;
	private int variableCounter = 0;

	// This is used to assign names to boilerplate values which have binding expressions in them, such as links.
	// These values are evaluated server side and stashed in the JSON response under a bogus binding.
	private int formatCounter = 0;

	private StringBuilder code = new StringBuilder(2048);
	private Deque<String> containerVariables = new ArrayDeque<>(16); // non-null elements
	
	/**
	 * Creates a renderer for generating SmartClient JavaScript for the supplied view.
	 *
	 * @param user The active user context.
	 * @param module The module containing the rendered document.
	 * @param document The document metadata to render.
	 * @param view The view metadata to render.
	 * @param uxui The active UX/UI profile.
	 * @param noCreateView Whether create/edit root container creation should be skipped.
	 *
	 * @return The protected.
	 */
	protected SmartClientViewRenderer(User user,
										Module module,
										Document document,
										View view,
										String uxui,
										boolean noCreateView) {
		super(user, module, document, view, uxui);
		this.noCreateView = noCreateView;
	}

	/**
	 * Returns the generated SmartClient JavaScript buffer.
	 *
	 * @return The mutable code buffer used during rendering.
	 */
	public StringBuilder getCode() {
		return code;
	}

	/**
	 * Starts SmartClient view rendering by creating the top-level container for the active view type.
	 *
	 * @param icon16x16Url The icon 16 x 16 url.
	 * @param icon32x32Url The icon 32 x 32 url.
	 */
	@Override
	public void renderView(String icon16x16Url, String icon32x32Url) {
		LOGGER.info("VIEW = {} for {}", view.getTitle(), document.getName());
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

	/**
	 * Rearranges the top-level layout to include a sidebar alongside the main view pane.
	 *
	 * @param sidebar The sidebar metadata to apply.
	 * @param invisibleConditionName The optional invisible condition for the wrapper container.
	 * @param variableName The JavaScript variable name to assign to the wrapper container.
	 */
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
	
	/**
	 * Finalizes SmartClient view rendering by attaching the generated view container to the root view.
	 *
	 * @param icon16x16Url The icon 16 x 16 url.
	 * @param icon32x32Url The icon 32 x 32 url.
	 */
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
	private Deque<Integer> tabNumbers = new ArrayDeque<>(4); // non-null elements

	/**
	 * Starts rendering a tab pane container.
	 *
	 * @param tabPane The tab pane metadata being rendered.
	 */
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

	/**
	 * Completes rendering of the current tab pane and adds it to the parent container.
	 *
	 * @param tabPane The tab pane metadata being rendered.
	 */
	@Override
	public void renderedTabPane(TabPane tabPane) {
		String variable = containerVariables.pop();
		code.append(containerVariables.peek()).append(".addContained(").append(variable).append(");\n");
		tabNumbers.pop();
	}

	/**
	 * Starts rendering a tab body container.
	 *
	 * @param title The localized tab title.
	 * @param icon16x16Url The optional 16x16 icon URL.
	 * @param tab The tab metadata being rendered.
	 */
	@Override
	public void renderTab(String title, String icon16x16Url, Tab tab) {
		String variable = "v" + variableCounter++;
		code.append("var ").append(variable).append("=isc.BizContainer.create({membersMargin:10,layoutMargin:10});\n");

		containerVariables.push(variable);
	}

	/**
	 * Completes rendering of a tab and registers it on the current tab pane.
	 *
	 * @param title The localized tab title.
	 * @param icon16x16Url The optional 16x16 icon URL.
	 * @param tab The tab metadata being rendered.
	 */
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

	/**
	 * Starts rendering a vertical box container.
	 *
	 * @param borderTitle The optional border title.
	 * @param vbox The vertical box metadata.
	 */
	@Override
	public void renderVBox(String borderTitle, VBox vbox) {
		vbox(borderTitle, vbox);
	}

	/**
	 * Renders a vertical box container with optional collapsible wrapping.
	 *
	 * @param borderTitle The optional border title.
	 * @param vbox The vertical box metadata.
	 */
	private void vbox(String borderTitle, VBox vbox) {
		String variable = "v" + variableCounter++;
		code.append("var ").append(variable).append("=isc.BizVBox.create({");

		// if collapsible, then make the inner vbox 100% width and height and do not put the border/title
		Collapsible collapsible = vbox.getCollapsible();
		if (collapsible != null) {
			validateCollapsible(collapsible, borderTitle);
			code.append("width:'100%',height:'100%',");
			if (vbox.getPixelPadding() == null) {
				code.append("layoutMargin:10,");
			}
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

		String collapsibleVar = collapsible(borderTitle, vbox, variable);
		code.append(containerVariables.peek()).append(".addContained(").append((collapsibleVar == null) ? variable : collapsibleVar).append(");\n");
		containerVariables.push(variable);
	}
	
	/**
	 * Completes rendering of the current vertical box container.
	 *
	 * @param borderTitle The optional border title.
	 * @param vbox The vertical box metadata.
	 */
	@Override
	public void renderedVBox(String borderTitle, VBox vbox) {
		containerVariables.pop();
	}

	/**
	 * Starts rendering a horizontal box container.
	 *
	 * @param borderTitle The optional border title.
	 * @param hbox The horizontal box metadata.
	 */
	@Override
	public void renderHBox(String borderTitle, HBox hbox) {
		String variable = "v" + variableCounter++;
		code.append("var ").append(variable).append("=isc.BizHBox.create({");
		
		// if collapsible, then make the inner hbox 100% width and height and do not put the border/title
		Collapsible collapsible = hbox.getCollapsible();
		if (collapsible != null) {
			validateCollapsible(collapsible, borderTitle);
			code.append("width:'100%',height:'100%',");
			if (hbox.getPixelPadding() == null) {
				code.append("layoutMargin:10,");
			}
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

		String collapsibleVar = collapsible(borderTitle, hbox, variable);
		code.append(containerVariables.peek()).append(".addContained(").append((collapsibleVar == null) ? variable : collapsibleVar).append(");\n");
		containerVariables.push(variable);
	}
	
	/**
	 * Completes rendering of the current horizontal box container.
	 *
	 * @param title The optional border title.
	 * @param bbox The horizontal box metadata.
	 */
	@Override
	public void renderedHBox(String title, HBox bbox) {
		containerVariables.pop();
	}

	/**
	 * Appends common spacing configuration for box containers.
	 *
	 * @param box The box metadata providing padding values.
	 */
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
	
	/**
	 * Wraps an item in a collapsible container when the box declares collapsible behaviour.
	 *
	 * @param borderTitle The localized border title.
	 * @param box The box metadata.
	 * @param itemVariable The JavaScript variable for the inner item.
	 * @return The collapsible wrapper variable name, or null when no wrapper is created.
	 */
	private String collapsible(String borderTitle, Box box, String itemVariable) {
		String result = null;
		Collapsible collapsible = box.getCollapsible();
		if (collapsible != null) {
			result = "v" + variableCounter++;
			code.append("var ").append(result).append("=isc.BizCollapsible.create({title:'").append(OWASP.escapeJsonString(borderTitle));
			code.append("',minimized:").append(collapsible.equals(Collapsible.closed) ? "true," : "false,");
			size(box, null, code);
			invisible(box.getInvisibleConditionName(), code);
			code.append("_view:view});\n");
			code.append(result).append(".addContained(").append(itemVariable).append(");\n");
		}
		return result;
	}
	
	private boolean viewHasAtLeastOneForm = false;
	private String formVariable = null;
	private VBox borderBox = null;

	/**
	 * Starts rendering a dynamic form and initializes its enclosing layout when needed.
	 *
	 * @param borderTitle The optional border title.
	 * @param form The form metadata being rendered.
	 */
	@Override
	public void renderForm(String borderTitle, Form form) {
		viewHasAtLeastOneForm = true;

		// If a form is defined with a border, then wrap the form definition in a vbox.
		// SC 8.2 couldn't cope with chrome and would draw the fieldset/border too small
		// for its content.
		Boolean border = form.getBorder();
		Collapsible collapsible = form.getCollapsible();
		
		validateCollapsible(collapsible, borderTitle);
		
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
			
			vbox(borderTitle, borderBox);
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

	/**
	 * Completes rendering of the current form and adds it to the container hierarchy.
	 *
	 * @param borderTitle The optional border title.
	 * @param form The form metadata being rendered.
	 */
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

	/**
	 * Appends a rendered form column width definition.
	 *
	 * @param column The form column metadata.
	 */
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

	/**
	 * Starts rendering a form row and emits required row separators.
	 *
	 * @param row The form row metadata being rendered.
	 */
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

	/**
	 * Starts rendering a SmartClient form item definition for the current form cell.
	 *
	 * @param label The label.
	 * @param requiredMessage The required message.
	 * @param help The help.
	 * @param showLabel The show label.
	 * @param colspan The colspan.
	 * @param item The item.
	 */
	@Override
	public void renderFormItem(String label,
								String requiredMessage,
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
			code.append("align:'").append(horizontalAlignment.toTextAlignmentString()).append("',");
		}
		horizontalAlignment = item.getLabelHorizontalAlignment();
		if (horizontalAlignment != null) {
			code.append("titleAlign:'").append(horizontalAlignment.toTextAlignmentString()).append("',");
		}
//		item.getVerticalAlignment()
//		item.getShowHelp()
//		code.append("{width:'*',");
	}

	/**
	 * Completes a SmartClient form item definition and advances internal form-column tracking.
	 *
	 * @param label The label.
	 * @param requiredMessage The required message.
	 * @param help The help.
	 * @param showLabel The show label.
	 * @param colspan The colspan.
	 * @param item The item.
	 */
	@Override
	public void renderedFormItem(String label,
									String requiredMessage,
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
	/**
	 * Closes the current SmartClient form row.
	 *
	 * @param row The row.
	 */

	@Override
	public void renderedFormRow(FormRow row) {
		code.setLength(code.length() - 2); // remove "},"
		code.append(",endRow:true},");
	}

	/**
	 * Starts rendering a button within a form item context.
	 *
	 * @param name The action name.
	 * @param label The localized button label.
	 * @param iconUrl The optional icon URL.
	 * @param iconStyleClass The optional icon style class.
	 * @param toolTip The optional tooltip text.
	 * @param confirmationText The optional confirmation message.
	 * @param action The action metadata.
	 * @param button The button widget metadata.
	 */
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

	/**
	 * Renders a standalone button and appends it to the current container.
	 *
	 * @param name The action name.
	 * @param label The localized button label.
	 * @param iconUrl The optional icon URL.
	 * @param iconStyleClass The optional icon style class.
	 * @param toolTip The optional tooltip text.
	 * @param confirmationText The optional confirmation message.
	 * @param action The action metadata.
	 * @param button The button widget metadata.
	 */
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

	/**
	 * Starts rendering a zoom-in control within a form item context.
	 *
	 * @param label The localized label.
	 * @param iconUrl The optional icon URL.
	 * @param iconStyleClass The optional icon style class.
	 * @param toolTip The optional tooltip text.
	 * @param zoomIn The zoom-in metadata.
	 */
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

	/**
	 * Renders a standalone zoom-in control and appends it to the current container.
	 *
	 * @param label The localized label.
	 * @param iconUrl The optional icon URL.
	 * @param iconStyleClass The optional icon style class.
	 * @param toolTip The optional tooltip text.
	 * @param zoomIn The zoom-in metadata.
	 */
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

	/**
	 * Renders a map display widget.
	 *
	 * @param map The map metadata.
	 */
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

	/**
	 * Renders a chart widget.
	 *
	 * @param chart The chart metadata.
	 */
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

	/**
	 * Records a geometry widget for deferred bound-column rendering.
	 *
	 * @param geometry The geometry metadata.
	 */
	@Override
	public void renderBoundColumnGeometry(Geometry geometry) {
		dataWidgetColumnInputWidget = geometry;
	}

	/**
	 * Completes bound-column geometry rendering.
	 *
	 * @param geometry The geometry metadata.
	 */
	@Override
	public void renderedBoundColumnGeometry(Geometry geometry) {
		// do nothing
	}

	/**
	 * Starts rendering a geometry input in a form.
	 *
	 * @param geometry The geometry metadata.
	 */
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

	/**
	 * Completes form geometry rendering.
	 *
	 * @param geometry The geometry metadata.
	 */
	@Override
	public void renderedFormGeometry(Geometry geometry) {
		// do nothing
	}

	/**
	 * Starts rendering a geometry map input in a form.
	 *
	 * @param geometry The geometry map metadata.
	 */
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

	/**
	 * Completes form geometry map rendering.
	 *
	 * @param geometry The geometry map metadata.
	 */
	@Override
	public void renderedFormGeometryMap(GeometryMap geometry) {
		// do nothing
	}

	/**
	 * Renders a dialog button placeholder inside a form item.
	 *
	 * @param label The localized button label.
	 * @param button The dialog button metadata.
	 */
	@Override
	public void renderFormDialogButton(String label, DialogButton button) {
		code.append("type:'blurb',defaultValue:'dialog button ");
		code.append(OWASP.escapeJsString(label)).append("',");
		disabled(button.getDisabledConditionName(), code);
		invisible(button.getInvisibleConditionName(), code);
	}

	/**
	 * Renders a standalone dialog button placeholder.
	 *
	 * @param label The localized button label.
	 * @param button The dialog button metadata.
	 */
	@Override
	public void renderDialogButton(String label, DialogButton button) {
		String variable = "v" + variableCounter++;
		code.append("var ").append(variable).append("=isc.BizLabel.create({value: '");
		code.append(OWASP.escapeJsString(label));
		code.append("'});\n");
		code.append(containerVariables.peek()).append(".addContained(").append(variable).append(");\n");
	}

	/**
	 * Renders a form spacer.
	 *
	 * @param spacer The spacer metadata.
	 */
	@Override
	public void renderFormSpacer(Spacer spacer) {
		code.append("type:'spacer',");
		size(spacer, null, code);
		invisible(spacer.getInvisibleConditionName(), code);
	}

	/**
	 * Renders a standalone layout spacer.
	 *
	 * @param spacer The spacer metadata.
	 */
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
	/**
	 * Renders a static image inside a form item.
	 *
	 * @param fileUrl The resolved static image URL.
	 * @param image The static image metadata.
	 */
	@Override
	public void renderFormStaticImage(String fileUrl, StaticImage image) {
		if (isCurrentWidgetShowLabel()) {
			String title = getCurrentWidgetLabel();
			if (title != null) {
				code.append("showTitle:true,title:\"").append(OWASP.escapeJsString(title)).append("\",");
			}
		}
		else {
			code.append("showTitle:false,");
		}
		code.append("type:'canvas',canvas:");
		addStaticImage(image);
		code.append(',');
	}

	/**
	 * Renders a static image for a container column.
	 *
	 * @param fileUrl The resolved static image URL.
	 * @param image The static image metadata.
	 */
	@Override
	public void renderContainerColumnStaticImage(String fileUrl, StaticImage image) {
		// markup is generated in the JSON data for a data grid container column static image
	}

	// TODO size, invisibility and binding
	/**
	 * Renders a standalone static image.
	 *
	 * @param fileUrl The resolved static image URL.
	 * @param image The static image metadata.
	 */
	@Override
	public void renderStaticImage(String fileUrl, StaticImage image) {
		String variable = "v" + variableCounter++;
		code.append("var ").append(variable).append('=');
		addStaticImage(image);
		code.append(";\n");
		code.append(containerVariables.peek()).append(".addContained(").append(variable).append(");\n");
	}

	/**
	 * Appends JavaScript to construct a static image component.
	 *
	 * @param image The static image metadata.
	 */
	private void addStaticImage(StaticImage image) {
		code.append("isc.BizImage.create({modoc:'").append(module.getName()).append('.').append(document.getName());
		code.append("',file:'").append(image.getRelativeFile()).append("',");
		size(image, null, code);
		removeTrailingComma(code);
		code.append("})");
	}

	/**
	 * Renders a dynamic image for a container column.
	 *
	 * @param image The dynamic image metadata.
	 */
	@Override
	public void renderContainerColumnDynamicImage(DynamicImage image) {
		// markup is generated in the JSON data for a data grid container column dynamic image
	}

	/**
	 * Renders a standalone dynamic image.
	 *
	 * @param image The dynamic image metadata.
	 */
	@Override
	public void renderDynamicImage(DynamicImage image) {
		String variable = "v" + variableCounter++;
		code.append("var ").append(variable).append('=');
		addImage(image);
		code.append(";\n");
		code.append(containerVariables.peek()).append(".addContained(").append(variable).append(");\n");
	}

	/**
	 * Appends JavaScript to construct a dynamic image component.
	 *
	 * @param image The dynamic image metadata.
	 */
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

	/**
	 * Renders a form link field.
	 *
	 * @param value The link value expression.
	 * @param link The link metadata.
	 */
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

	/**
	 * Renders a container-column link.
	 *
	 * @param value The link value expression.
	 * @param link The link metadata.
	 */
	@Override
	public void renderContainerColumnLink(String value, Link link) {
		// markup is generated in the JSON data for a data grid container column link
	}

	/**
	 * Renders a standalone link widget.
	 *
	 * @param value The link value expression.
	 * @param link The link metadata.
	 */
	@Override
	public void renderLink(String value, Link link) {
		// TODO Implement later
	}

	/**
	 * Creates a temporary label using blurb metadata.
	 *
	 * @param blurb The blurb metadata source.
	 * @return A label configured from the blurb.
	 */
	private static Label makeNewLabelFromBlurb(Blurb blurb) {
		Label result = new Label();
		result.setValue(blurb.getMarkup());
		result.setPixelWidth(blurb.getPixelWidth());
		result.setPixelHeight(blurb.getPixelHeight());
		result.setTextAlignment(blurb.getTextAlignment());
		result.setInvisibleConditionName(blurb.getInvisibleConditionName());
		return result;
	}

	/**
	 * Renders blurb markup as a form label.
	 *
	 * @param markup The blurb markup.
	 * @param blurb The blurb metadata.
	 */
	@Override
	public void renderFormBlurb(String markup, Blurb blurb) {
		renderFormLabel(markup, BindUtil.containsSkyveExpressions(markup), makeNewLabelFromBlurb(blurb));
	}

	/**
	 * Renders blurb markup as a container-column label.
	 *
	 * @param markup The blurb markup.
	 * @param blurb The blurb metadata.
	 */
	@Override
	public void renderContainerColumnBlurb(String markup, Blurb blurb) {
		renderContainerColumnLabel(markup, makeNewLabelFromBlurb(blurb));
	}

	/**
	 * Renders blurb markup as a standalone label.
	 *
	 * @param markup The blurb markup.
	 * @param blurb The blurb metadata.
	 */
	@Override
	public void renderBlurb(String markup, Blurb blurb) {
		renderLabel(markup, BindUtil.containsSkyveExpressions(markup), makeNewLabelFromBlurb(blurb));
	}

	/**
	 * Renders a label inside a form item.
	 *
	 * @param value The label value.
	 * @param boundValue Whether the value contains binding expressions.
	 * @param label The label metadata.
	 */
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
			code.append("textAlign:'").append(alignment.toTextAlignmentString()).append("',");
		}
		size(label, null, code);
		invisible(label.getInvisibleConditionName(), code);
	}

	/**
	 * Renders a label for a container column.
	 *
	 * @param value The label value.
	 * @param label The label metadata.
	 */
	@Override
	public void renderContainerColumnLabel(String value, Label label) {
		// markup is generated in the JSON data for a data grid container column label or a dynamic form-based value
	}

	/**
	 * Renders a standalone label component.
	 *
	 * @param value The label value.
	 * @param boundValue Whether the value contains binding expressions.
	 * @param label The label metadata.
	 */
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
			code.append("textAlign:'").append(alignment.toTextAlignmentString()).append("',");
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

	/**
	 * Renders a progress bar placeholder inside a form.
	 *
	 * @param progressBar The progress bar metadata.
	 */
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

	/**
	 * Starts rendering a list grid.
	 *
	 * @param title The list grid title.
	 * @param aggregateQuery Whether the backing query is aggregate.
	 * @param grid The list grid metadata.
	 */
	@Override
	public void renderListGrid(String title, boolean aggregateQuery, ListGrid grid) {
		renderListWidget(grid);
		renderGrid(grid);
	}

	/**
	 * Renders a projected list-grid column.
	 *
	 * @param column The projected query column metadata.
	 */
	@Override
	public void renderListGridProjectedColumn(MetaDataQueryProjectedColumn column) {
		// TODO Auto-generated method stub

	}

	/**
	 * Renders a content list-grid column.
	 *
	 * @param column The content query column metadata.
	 */
	@Override
	public void renderListGridContentColumn(MetaDataQueryContentColumn column) {
		// TODO Auto-generated method stub

	}

	/**
	 * Completes rendering of a list grid.
	 *
	 * @param title The list grid title.
	 * @param aggregateQuery Whether the backing query is aggregate.
	 * @param grid The list grid metadata.
	 */
	@Override
	public void renderedListGrid(String title, boolean aggregateQuery, ListGrid grid) {
		appendFilterParameters(grid.getFilterParameters(), grid.getParameters(), code);
		renderedListWidget();
	}

	/**
	 * Starts rendering a list repeater.
	 *
	 * @param title The repeater title.
	 * @param repeater The list repeater metadata.
	 */
	@Override
	public void renderListRepeater(String title, ListRepeater repeater) {
		renderListWidget(repeater);
		code.append("isRepeater:true,");
		code.append("showColumnHeaders:").append(Boolean.TRUE.equals(repeater.getShowColumnHeaders())).append(',');
		code.append("showGrid:").append(Boolean.TRUE.equals(repeater.getShowGrid())).append(',');
	}

	/**
	 * Renders a projected list-repeater column.
	 *
	 * @param column The projected query column metadata.
	 */
	@Override
	public void renderListRepeaterProjectedColumn(MetaDataQueryProjectedColumn column) {
		// TODO Auto-generated method stub

	}

	/**
	 * Renders a content list-repeater column.
	 *
	 * @param column The content query column metadata.
	 */
	@Override
	public void renderListRepeaterContentColumn(MetaDataQueryContentColumn column) {
		// TODO Auto-generated method stub

	}

	/**
	 * Completes rendering of a list repeater.
	 *
	 * @param title The repeater title.
	 * @param repeater The list repeater metadata.
	 */
	@Override
	public void renderedListRepeater(String title, ListRepeater repeater) {
		appendFilterParameters(repeater.getFilterParameters(), repeater.getParameters(), code);
		renderedListWidget();
	}

	/**
	 * Starts rendering a tree grid.
	 *
	 * @param title The tree grid title.
	 * @param grid The tree grid metadata.
	 */
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

	/**
	 * Renders a projected tree-grid column.
	 *
	 * @param column The projected query column metadata.
	 */
	@Override
	public void renderTreeGridProjectedColumn(MetaDataQueryProjectedColumn column) {
		// TODO Auto-generated method stub

	}

	/**
	 * Renders a content tree-grid column.
	 *
	 * @param column The content query column metadata.
	 */
	@Override
	public void renderTreeGridContentColumn(MetaDataQueryContentColumn column) {
		// TODO Auto-generated method stub

	}

	/**
	 * Completes rendering of a tree grid.
	 *
	 * @param title The tree grid title.
	 * @param grid The tree grid metadata.
	 */
	@Override
	public void renderedTreeGrid(String title, TreeGrid grid) {
		appendFilterParameters(grid.getFilterParameters(), grid.getParameters(), code);
		renderedListWidget();
	}

	/**
	 * Starts rendering common list-widget infrastructure.
	 *
	 * @param widget The list widget metadata.
	 */
	private void renderListWidget(AbstractListWidget widget) {
		String queryName = widget.getQueryName();
		String modelName = widget.getModelName();
		String dataSourceId = null;
		if ((module == null) || (document == null)) {
			throw new MetaDataException("Cannot render list widget without module and document context");
		}
		if (queryName != null) { // its a query
			MetaDataQueryDefinition query = module.getNullSafeMetaDataQuery(queryName);
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

	/**
	 * Appends common list-grid properties shared by list and tree grids.
	 *
	 * @param grid The list-grid metadata.
	 */
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
		if (Boolean.FALSE.equals(grid.getShowFlag())) {
			code.append("showFlag:false,");
		}
		if (Boolean.FALSE.equals(grid.getAutoPopulate())) {
			code.append("autoPopulate:false,");
		}
	}

	/**
	 * Completes rendering for the current list widget.
	 */
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

	/**
	 * Starts rendering a data grid widget.
	 *
	 * @param title The grid title.
	 * @param grid The data-grid metadata.
	 */
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

	/**
	 * Completes rendering of a data grid widget.
	 *
	 * @param title The grid title.
	 * @param grid The data-grid metadata.
	 */
	@Override
	public void renderedDataGrid(String title, DataGrid grid) {
		renderedDataWidget();
	}

	/**
	 * Starts rendering a data repeater widget.
	 *
	 * @param title The repeater title.
	 * @param repeater The repeater metadata.
	 */
	@Override
	public void renderDataRepeater(String title, DataRepeater repeater) {
		renderDataWidget(repeater);
		code.append("isRepeater:true,");
		code.append("showColumnHeaders:").append(Boolean.TRUE.equals(repeater.getShowColumnHeaders()));
		code.append(",showGrid:").append(Boolean.TRUE.equals(repeater.getShowGrid()));
		code.append(",_fields:[");
	}

	/**
	 * Completes rendering of a data repeater widget.
	 *
	 * @param title The repeater title.
	 * @param repeater The repeater metadata.
	 */
	@Override
	public void renderedDataRepeater(String title, DataRepeater repeater) {
		renderedDataWidget();
	}

	/**
	 * Starts rendering shared data-widget infrastructure.
	 *
	 * @param widget The data-widget metadata.
	 */
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
		if ((relation instanceof Collection collection) && Boolean.TRUE.equals(collection.getOrdered())) {
			code.append("_ordinal:'").append(Bean.ORDINAL_NAME).append("',");
		}
		size(widget, DEFAULT_MIN_HEIGHT_IN_PIXELS, code);
		invisible(widget.getInvisibleConditionName(), code);
		dataWidgetFieldsIncomplete = true;
	}

	/**
	 * Completes rendering for the current data widget.
	 */
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

	/**
	 * Starts rendering a bound data-grid column.
	 *
	 * @param title The column title.
	 * @param column The bound column metadata.
	 */
	@Override
	public void renderDataGridBoundColumn(String title, DataGridBoundColumn column) {
		// do nothing
	}

	/**
	 * Starts rendering a bound data-repeater column.
	 *
	 * @param title The column title.
	 * @param column The bound column metadata.
	 */
	@Override
	public void renderDataRepeaterBoundColumn(String title, DataGridBoundColumn column) {
		renderDataGridBoundColumn(title, column);
	}

	/**
	 * Completes rendering a bound data-grid column.
	 *
	 * @param title The column title.
	 * @param column The bound column metadata.
	 */
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

	/**
	 * Completes rendering a bound data-repeater column.
	 *
	 * @param title The column title.
	 * @param column The bound column metadata.
	 */
	@Override
	public void renderedDataRepeaterBoundColumn(String title, DataGridBoundColumn column) {
		renderedDataGridBoundColumn(title, column);
	}

	/**
	 * Starts rendering a data-grid container column.
	 *
	 * @param title The column title.
	 * @param column The container column metadata.
	 */
	@Override
	public void renderDataGridContainerColumn(String title, DataGridContainerColumn column) {
		code.append("{name:'_").append(formatCounter++);
		code.append("',type:'text',formatCellValue:'value;',canEdit:false,title:'");

		code.append((title == null) ? " " : OWASP.escapeJsString(title)).append('\'');
		HorizontalAlignment alignment = column.getAlignment();
		if (alignment != null) {
			code.append(",align:'").append(alignment.toTextAlignmentString()).append('\'');
		}
		Integer width = column.getPixelWidth();
		if (width != null) {
			code.append(",width:").append(width);
		}
		code.append("},");
	}

	/**
	 * Starts rendering a data-repeater container column.
	 *
	 * @param title The column title.
	 * @param column The container column metadata.
	 */
	@Override
	public void renderDataRepeaterContainerColumn(String title, DataGridContainerColumn column) {
		renderDataGridContainerColumn(title, column);
	}

	/**
	 * Completes rendering a data-grid container column.
	 *
	 * @param title The column title.
	 * @param column The container column metadata.
	 */
	@Override
	public void renderedDataGridContainerColumn(String title, DataGridContainerColumn column) {
		// do nothing
	}

	/**
	 * Completes rendering a data-repeater container column.
	 *
	 * @param title The column title.
	 * @param column The container column metadata.
	 */
	@Override
	public void renderedDataRepeaterContainerColumn(String title, DataGridContainerColumn column) {
		renderedDataGridContainerColumn(title, column);
	}

	/**
	 * Starts rendering a bound-column checkbox.
	 *
	 * @param checkBox The checkbox metadata.
	 */
	@Override
	public void renderBoundColumnCheckBox(CheckBox checkBox) {
		dataWidgetColumnInputWidget = checkBox;
	}

	/**
	 * Renders a checkbox form item.
	 *
	 * @param checkBox The checkbox metadata.
	 */
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

	/**
	 * Completes bound-column checkbox rendering.
	 *
	 * @param checkBox The checkbox metadata.
	 */
	@Override
	public void renderedBoundColumnCheckBox(CheckBox checkBox) {
		// do nothing
	}

	/**
	 * Completes form checkbox rendering.
	 *
	 * @param checkBox The checkbox metadata.
	 */
	@Override
	public void renderedFormCheckBox(CheckBox checkBox) {
		// do nothing
	}

	// TODO implement this - does this need size? probably
	/**
	 * Renders a check-membership placeholder component.
	 *
	 * @param membership The check-membership metadata.
	 */
	@Override
	public void renderCheckMembership(CheckMembership membership) {
		String variable = "v" + variableCounter++;
		code.append("var ").append(variable).append("=isc.BizLabel.create({value:'");
		code.append("check membership").append(membership.getBinding());
		code.append("'});\n");
		code.append(containerVariables.peek()).append(".addContained(").append(variable).append(");\n");
	}

	/**
	 * Completes check-membership rendering.
	 *
	 * @param membership The check-membership metadata.
	 */
	@Override
	public void renderedCheckMembership(CheckMembership membership) {
		// do nothing - until implemented properly
	}

	/**
	 * Starts rendering a bound-column colour picker.
	 *
	 * @param colour The colour picker metadata.
	 */
	@Override
	public void renderBoundColumnColourPicker(ColourPicker colour) {
		dataWidgetColumnInputWidget = colour;
	}

	/**
	 * Renders a colour picker form item.
	 *
	 * @param colour The colour picker metadata.
	 */
	@Override
	public void renderFormColourPicker(ColourPicker colour) {
		preProcessFormItem(colour, "color");
		size(colour, null, code);
		disabled(colour.getDisabledConditionName(), code);
		invisible(colour.getInvisibleConditionName(), code);
	}

	/**
	 * Completes bound-column colour picker rendering.
	 *
	 * @param colour The colour picker metadata.
	 */
	@Override
	public void renderedBoundColumnColourPicker(ColourPicker colour) {
		// do nothing
	}

	/**
	 * Completes form colour picker rendering.
	 *
	 * @param colour The colour picker metadata.
	 */
	@Override
	public void renderedFormColourPicker(ColourPicker colour) {
		// do nothing
	}

	/**
	 * Starts rendering a bound-column combo field.
	 *
	 * @param combo The combo metadata.
	 */
	@Override
	public void renderBoundColumnCombo(Combo combo) {
		dataWidgetColumnInputWidget = combo;
	}

	/**
	 * Renders a combo form item.
	 *
	 * @param combo The combo metadata.
	 */
	@Override
	public void renderFormCombo(Combo combo) {
		preProcessFormItem(combo, "select");
		size(combo, null, code);
		disabled(combo.getDisabledConditionName(), code);
		invisible(combo.getInvisibleConditionName(), code);
	}

	/**
	 * Completes bound-column combo rendering.
	 *
	 * @param combo The combo metadata.
	 */
	@Override
	public void renderedBoundColumnCombo(Combo combo) {
		// do nothing
	}

	/**
	 * Completes form combo rendering.
	 *
	 * @param combo The combo metadata.
	 */
	@Override
	public void renderedFormCombo(Combo combo) {
		// do nothing
	}

	/**
	 * Starts rendering a bound-column content image.
	 *
	 * @param image The content-image metadata.
	 */
	@Override
	public void renderBoundColumnContentImage(ContentImage image) {
		dataWidgetColumnInputWidget = image;
	}

	/**
	 * Renders a container-column content image.
	 *
	 * @param image The content-image metadata.
	 */
	@Override
	public void renderContainerColumnContentImage(ContentImage image) {
		// markup is generated in the JSON data for a data grid container column content image
	}

	/**
	 * Renders a content image form item.
	 *
	 * @param image The content-image metadata.
	 */
	@Override
	public void renderFormContentImage(ContentImage image) {
		preProcessFormItem(image, "bizContentImage");
		size(image, null, code);
		disabled(image.getDisabledConditionName(), code);
		invisible(image.getInvisibleConditionName(), code);
		editable(image.getEditable(), code);
		code.append("showMarkup:").append((! Boolean.FALSE.equals(image.getShowMarkup())) ? "true," : "false,");
	}

	/**
	 * Starts rendering a bound-column content link.
	 *
	 * @param value The link value.
	 * @param link The content-link metadata.
	 */
	@Override
	public void renderBoundColumnContentLink(String value, ContentLink link) {
		dataWidgetColumnInputWidget = link;
	}

	/**
	 * Renders a content link form item.
	 *
	 * @param value The link value.
	 * @param link The content-link metadata.
	 */
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

	/**
	 * Renders a content signature form item.
	 *
	 * @param signature The content-signature metadata.
	 */
	@Override
	public void renderFormContentSignature(ContentSignature signature) {
		// TODO not implemented for SC yet - use a ContentImage for now
		preProcessFormItem(signature, "bizContentImage");
		size(signature, null, code);
		disabled(signature.getDisabledConditionName(), code);
		invisible(signature.getInvisibleConditionName(), code);
		editable(Boolean.TRUE, code);
	}

	/**
	 * Starts rendering a bound-column HTML editor.
	 *
	 * @param html The HTML metadata.
	 */
	@Override
	public void renderBoundColumnHTML(HTML html) {
		dataWidgetColumnInputWidget = html;
	}

	/**
	 * Renders an HTML form item.
	 *
	 * @param html The HTML metadata.
	 */
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

	/**
	 * Starts rendering a list-membership widget.
	 *
	 * @param candidatesHeading The optional candidates heading.
	 * @param membersHeading The optional members heading.
	 * @param membership The list-membership metadata.
	 */
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
		if ((relation instanceof Collection collection) && Boolean.TRUE.equals(collection.getOrdered())) {
			code.append(",_ordinal:'").append(Bean.ORDINAL_NAME).append('\'');
		}
		code.append(",_view:view,");
		size(membership, DEFAULT_MIN_HEIGHT_IN_PIXELS, code);
		disabled(membership.getDisabledConditionName(), code);
		invisible(membership.getInvisibleConditionName(), code);

		containerVariables.push(variable);
	}

	/**
	 * Completes rendering of a list-membership widget.
	 *
	 * @param candidatesHeading The optional candidates heading.
	 * @param membersHeading The optional members heading.
	 * @param membership The list-membership metadata.
	 */
	@Override
	public void renderedListMembership(String candidatesHeading, String membersHeading, ListMembership membership) {
		removeTrailingComma(code);
		code.append("});\n");

		String variable = containerVariables.pop();
		code.append(containerVariables.peek()).append(".addContained(").append(variable).append(");\n");
	}

	/**
	 * Renders a comparison widget.
	 *
	 * @param comparison The comparison metadata.
	 */
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

	/**
	 * Starts rendering a bound-column lookup description.
	 *
	 * @param query The lookup query metadata.
	 * @param canCreate Whether create is allowed.
	 * @param canUpdate Whether update is allowed.
	 * @param descriptionBinding The description binding.
	 * @param lookup The lookup metadata.
	 */
	@Override
	public void renderBoundColumnLookupDescription(MetaDataQueryDefinition query,
													boolean canCreate,
													boolean canUpdate,
													String descriptionBinding,
													LookupDescription lookup) {
		dataWidgetColumnInputWidget = lookup;
	}

	/**
	 * Renders a lookup description form item.
	 *
	 * @param query The lookup query metadata.
	 * @param canCreate Whether create is allowed.
	 * @param canUpdate Whether update is allowed.
	 * @param descriptionBinding The description binding.
	 * @param lookup The lookup metadata.
	 */
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

			/**
			 * Completes bound-column lookup-description rendering.
			 *
			 * @param query The lookup query metadata.
			 * @param canCreate Whether create is allowed.
			 * @param canUpdate Whether update is allowed.
			 * @param descriptionBinding The description binding.
			 * @param lookup The lookup metadata.
			 */
	@Override
	public void renderedBoundColumnLookupDescription(MetaDataQueryDefinition query,
														boolean canCreate,
														boolean canUpdate,
														String descriptionBinding,
														LookupDescription lookup) {
		// do nothing
	}

		/**
		 * Completes form lookup-description rendering.
		 *
		 * @param query The lookup query metadata.
		 * @param canCreate Whether create is allowed.
		 * @param canUpdate Whether update is allowed.
		 * @param descriptionBinding The description binding.
		 * @param lookup The lookup metadata.
		 */
	@Override
	public void renderedFormLookupDescription(MetaDataQueryDefinition query,
												boolean canCreate,
												boolean canUpdate,
												String descriptionBinding,
												LookupDescription lookup) {
		// do nothing
	}

	/**
	 * Starts rendering a bound-column password field.
	 *
	 * @param password The password metadata.
	 */
	@Override
	public void renderBoundColumnPassword(Password password) {
		dataWidgetColumnInputWidget = password;
	}

	/**
	 * Renders a password form item.
	 *
	 * @param password The password metadata.
	 */
	@Override
	public void renderFormPassword(Password password) {
		preProcessFormItem(password, "password");
		size(password, null, code);
		// Security settings
		code.append("autoComplete:'none',browserAutoCapitalize:false,browserAutoCorrect:false,browserSpellCheck:false,");
		disabled(password.getDisabledConditionName(), code);
		invisible(password.getInvisibleConditionName(), code);
	}

	/**
	 * Completes bound-column password rendering.
	 *
	 * @param password The password metadata.
	 */
	@Override
	public void renderedBoundColumnPassword(Password password) {
		// do nothing
	}

	/**
	 * Completes form password rendering.
	 *
	 * @param password The password metadata.
	 */
	@Override
	public void renderedFormPassword(Password password) {
		// do nothing
	}

	/**
	 * Starts rendering a bound-column radio control.
	 *
	 * @param radio The radio metadata.
	 */
	@Override
	public void renderBoundColumnRadio(Radio radio) {
		dataWidgetColumnInputWidget = radio;
	}

	/**
	 * Renders a radio form item.
	 *
	 * @param radio The radio metadata.
	 */
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

	/**
	 * Completes bound-column radio rendering.
	 *
	 * @param radio The radio metadata.
	 */
	@Override
	public void renderedBoundColumnRadio(Radio radio) {
		// do nothing
	}

	/**
	 * Completes form radio rendering.
	 *
	 * @param radio The radio metadata.
	 */
	@Override
	public void renderedFormRadio(Radio radio) {
		// do nothing
	}

	/**
	 * Starts rendering a bound-column rich-text field.
	 *
	 * @param text The rich-text metadata.
	 */
	@Override
	public void renderBoundColumnRichText(RichText text) {
		dataWidgetColumnInputWidget = text;
	}

	/**
	 * Renders a rich-text form item.
	 *
	 * @param text The rich-text metadata.
	 */
	@Override
	public void renderFormRichText(RichText text) {
		preProcessFormItem(text, "richText");
		size(text, DEFAULT_MIN_HEIGHT_IN_PIXELS, code);
		disabled(text.getDisabledConditionName(), code);
		invisible(text.getInvisibleConditionName(), code);
	}

	/**
	 * Completes bound-column rich-text rendering.
	 *
	 * @param richText The rich-text metadata.
	 */
	@Override
	public void renderedBoundColumnRichText(RichText richText) {
		// do nothing
	}

	/**
	 * Completes form rich-text rendering.
	 *
	 * @param richText The rich-text metadata.
	 */
	@Override
	public void renderedFormRichText(RichText richText) {
		// do nothing
	}

	/**
	 * Starts rendering a bound-column slider.
	 *
	 * @param slider The slider metadata.
	 */
	@Override
	public void renderBoundColumnSlider(Slider slider) {
		dataWidgetColumnInputWidget = slider;
	}

	/**
	 * Renders a slider form item.
	 *
	 * @param slider The slider metadata.
	 */
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

	/**
	 * Completes bound-column slider rendering.
	 *
	 * @param slider The slider metadata.
	 */
	@Override
	public void renderedBoundColumnSlider(Slider slider) {
		// do nothing
	}

	/**
	 * Completes form slider rendering.
	 *
	 * @param slider The slider metadata.
	 */
	@Override
	public void renderedFormSlider(Slider slider) {
		// do nothing
	}

	/**
	 * Starts rendering a bound-column spinner.
	 *
	 * @param spinner The spinner metadata.
	 */
	@Override
	public void renderBoundColumnSpinner(Spinner spinner) {
		dataWidgetColumnInputWidget = spinner;
	}

	/**
	 * Renders a spinner form item.
	 *
	 * @param spinner The spinner metadata.
	 */
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

	/**
	 * Completes bound-column spinner rendering.
	 *
	 * @param spinner The spinner metadata.
	 */
	@Override
	public void renderedBoundColumnSpinner(Spinner spinner) {
		// do nothing
	}

	/**
	 * Completes form spinner rendering.
	 *
	 * @param spinner The spinner metadata.
	 */
	@Override
	public void renderedFormSpinner(Spinner spinner) {
		// do nothing
	}

	/**
	 * Starts rendering a bound-column text area.
	 *
	 * @param text The text-area metadata.
	 */
	@Override
	public void renderBoundColumnTextArea(TextArea text) {
		dataWidgetColumnInputWidget = text;
	}

	/**
	 * Renders a text-area form item.
	 *
	 * @param text The text-area metadata.
	 */
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

	/**
	 * Completes bound-column text-area rendering.
	 *
	 * @param text The text-area metadata.
	 */
	@Override
	public void renderedBoundColumnTextArea(TextArea text) {
		// do nothing
	}

	/**
	 * Completes form text-area rendering.
	 *
	 * @param text The text-area metadata.
	 */
	@Override
	public void renderedFormTextArea(TextArea text) {
		// do nothing
	}

	/**
	 * Starts rendering a bound-column text field.
	 *
	 * @param text The text-field metadata.
	 */
	@Override
	public void renderBoundColumnTextField(TextField text) {
		dataWidgetColumnInputWidget = text;
	}

	/**
	 * Renders a text-field form item.
	 *
	 * @param text The text-field metadata.
	 */
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

	/**
	 * Completes bound-column text-field rendering.
	 *
	 * @param text The text-field metadata.
	 */
	@Override
	public void renderedBoundColumnTextField(TextField text) {
		// do nothing
	}

	/**
	 * Completes form text-field rendering.
	 *
	 * @param text The text-field metadata.
	 */
	@Override
	public void renderedFormTextField(TextField text) {
		// do nothing
	}
	
	/**
	 * Renders an inject script within a form item.
	 *
	 * @param inject The inject metadata.
	 */
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

	/**
	 * Renders an inject script in the current output stream.
	 *
	 * @param inject The inject metadata.
	 */
	@Override
	public void renderInject(Inject inject) {
		code.append(inject.getScript());
	}

	/**
	 * Renders a custom action button using metadata-driven visibility, disablement, and parameters.
	 *
	 * @param name The name.
	 * @param label The label.
	 * @param iconUrl The icon url.
	 * @param iconStyleClass The icon style class.
	 * @param toolTip The tool tip.
	 * @param confirmationText The confirmation text.
	 * @param action The action.
	 */
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

	/**
	 * Renders the implicit add action button and wires it to the SmartClient add handler.
	 *
	 * @param name The name.
	 * @param label The label.
	 * @param iconUrl The icon url.
	 * @param iconStyleClass The icon style class.
	 * @param toolTip The tool tip.
	 * @param confirmationText The confirmation text.
	 * @param action The action.
	 */
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

	/**
	 * Renders the implicit remove action button and passes through delete capability flags.
	 *
	 * @param name The name.
	 * @param label The label.
	 * @param iconUrl The icon url.
	 * @param iconStyleClass The icon style class.
	 * @param toolTip The tool tip.
	 * @param confirmationText The confirmation text.
	 * @param action The action.
	 * @param canDelete The can delete.
	 */
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

	/**
	 * Renders the implicit zoom-out action button.
	 *
	 * @param name The name.
	 * @param label The label.
	 * @param iconUrl The icon url.
	 * @param iconStyleClass The icon style class.
	 * @param toolTip The tool tip.
	 * @param confirmationText The confirmation text.
	 * @param action The action.
	 */
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

	/**
	 * Renders the implicit navigate action button.
	 *
	 * @param name The name.
	 * @param label The label.
	 * @param iconUrl The icon url.
	 * @param iconStyleClass The icon style class.
	 * @param toolTip The tool tip.
	 * @param confirmationText The confirmation text.
	 * @param action The action.
	 */
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

	/**
	 * Renders the implicit OK action button.
	 *
	 * @param name The name.
	 * @param label The label.
	 * @param iconUrl The icon url.
	 * @param iconStyleClass The icon style class.
	 * @param toolTip The tool tip.
	 * @param confirmationText The confirmation text.
	 * @param action The action.
	 */
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

	/**
	 * Renders the implicit save action button.
	 *
	 * @param name The name.
	 * @param label The label.
	 * @param iconUrl The icon url.
	 * @param iconStyleClass The icon style class.
	 * @param toolTip The tool tip.
	 * @param confirmationText The confirmation text.
	 * @param action The action.
	 */
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

	/**
	 * Renders the implicit cancel action button.
	 *
	 * @param name The name.
	 * @param label The label.
	 * @param iconUrl The icon url.
	 * @param iconStyleClass The icon style class.
	 * @param toolTip The tool tip.
	 * @param confirmationText The confirmation text.
	 * @param action The action.
	 */
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

	/**
	 * Renders the implicit delete action button.
	 *
	 * @param name The name.
	 * @param label The label.
	 * @param iconUrl The icon url.
	 * @param iconStyleClass The icon style class.
	 * @param toolTip The tool tip.
	 * @param confirmationText The confirmation text.
	 * @param action The action.
	 */
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

	/**
	 * Renders the implicit report action button.
	 *
	 * @param name The name.
	 * @param label The label.
	 * @param iconUrl The icon url.
	 * @param iconStyleClass The icon style class.
	 * @param toolTip The tool tip.
	 * @param confirmationText The confirmation text.
	 * @param action The action.
	 */
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

	/**
	 * Renders the implicit biz-export action button.
	 *
	 * @param name The name.
	 * @param label The label.
	 * @param iconUrl The icon url.
	 * @param iconStyleClass The icon style class.
	 * @param toolTip The tool tip.
	 * @param confirmationText The confirmation text.
	 * @param action The action.
	 */
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

	/**
	 * Renders the implicit biz-import action button.
	 *
	 * @param name The name.
	 * @param label The label.
	 * @param iconUrl The icon url.
	 * @param iconStyleClass The icon style class.
	 * @param toolTip The tool tip.
	 * @param confirmationText The confirmation text.
	 * @param action The action.
	 */
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

	/**
	 * Renders the implicit download action button.
	 *
	 * @param name The name.
	 * @param label The label.
	 * @param iconUrl The icon url.
	 * @param iconStyleClass The icon style class.
	 * @param toolTip The tool tip.
	 * @param confirmationText The confirmation text.
	 * @param action The action.
	 */
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

	/**
	 * Renders the implicit upload action button.
	 *
	 * @param name The name.
	 * @param label The label.
	 * @param iconUrl The icon url.
	 * @param iconStyleClass The icon style class.
	 * @param toolTip The tool tip.
	 * @param confirmationText The confirmation text.
	 * @param action The action.
	 */
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

	/**
	 * Renders the implicit new action button.
	 *
	 * @param name The name.
	 * @param label The label.
	 * @param iconUrl The icon url.
	 * @param iconStyleClass The icon style class.
	 * @param toolTip The tool tip.
	 * @param confirmationText The confirmation text.
	 * @param action The action.
	 */
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

	/**
	 * Renders the implicit edit action button.
	 *
	 * @param name The name.
	 * @param label The label.
	 * @param iconUrl The icon url.
	 * @param iconStyleClass The icon style class.
	 * @param toolTip The tool tip.
	 * @param confirmationText The confirmation text.
	 * @param action The action.
	 */
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

	/**
	 * Renders the implicit print action button.
	 *
	 * @param name The name.
	 * @param label The label.
	 * @param iconUrl The icon url.
	 * @param iconStyleClass The icon style class.
	 * @param toolTip The tool tip.
	 * @param confirmationText The confirmation text.
	 * @param action The action.
	 */
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
	
	/**
	 * Emits deferred server-side callback wrappers for async add/edit/remove handlers.
	 *
	 * <p>Side effects: appends callback method definitions to the generated JavaScript buffer
	 * when a corresponding handler scope is active.
	 */
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
	
	/**
	 * Emits JavaScript that dispatches a server-side action callback for the current event context.
	 *
	 * @param action The action.
	 * @param server The server.
	 */
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
	
	/**
	 * Starts an on-change handler body for the current widget context.
	 *
	 * @param changeable The changeable.
	 * @param parentVisible The parent visible.
	 * @param parentEnabled The parent enabled.
	 */
	@Override
	public void visitOnChangedEventHandler(Changeable changeable, boolean parentVisible, boolean parentEnabled) {
		if (getCurrentForm() == null) {
			code.append("changed:function(){var view=this._view;");
		}
		else {
			code.append("changed:function(form,item,value){var view=form._view;");
		}
	}
	
	/**
	 * Closes the on-change handler body.
	 *
	 * @param changeable The changeable.
	 * @param parentVisible The parent visible.
	 * @param parentEnabled The parent enabled.
	 */
	@Override
	public void visitedOnChangedEventHandler(Changeable changeable, boolean parentVisible, boolean parentEnabled) {
		code.append("},");
	}
	
	/**
	 * Starts an on-focus handler that short-circuits while requests are pending.
	 *
	 * @param blurable The blurable.
	 * @param parentVisible The parent visible.
	 * @param parentEnabled The parent enabled.
	 */
	@Override
	public void visitOnFocusEventHandler(Focusable blurable, boolean parentVisible, boolean parentEnabled) {
		// Note the test to short circuit focus event processing whilst requests are pending to stop loops with multiple fields.
		code.append("editorEnter:function(form,item,value){if((!isc.RPCManager.requestsArePending())&&item.validate()){var view=form._view;");
	}
	
	/**
	 * Closes the on-focus handler body.
	 *
	 * @param blurable The blurable.
	 * @param parentVisible The parent visible.
	 * @param parentEnabled The parent enabled.
	 */
	@Override
	public void visitedOnFocusEventHandler(Focusable blurable, boolean parentVisible, boolean parentEnabled) {
		code.append("}},");
	}

	// indicates that we are blurring or selecting and we need to call special methods
	// to potentially serialize calls to button actions after editorExit.
	private boolean visitingAsync = false;
	
	/**
	 * Starts blur/edit-exit handlers and enables async sequencing for blurry actions.
	 *
	 * @param blurable The blurable.
	 * @param parentVisible The parent visible.
	 * @param parentEnabled The parent enabled.
	 */
	@Override
	public void visitOnBlurEventHandler(Focusable blurable, boolean parentVisible, boolean parentEnabled) {
		visitingAsync = true;
		
		// This fires before the BizButton action() method if a button was clicked
		// Note the test to short circuit blur event processing whilst requests are pending to stop loops with multiple fields.
		code.append("blur:function(form,item){if(isc.RPCManager.requestsArePending()){form._view._blurry=null;}else{form._view._blurry=item;}},");
		// This is called before or after the BizButton action depending on the browser.
		// Note the test to short circuit blur event processing whilst requests are pending to stop loops with multiple fields.
		code.append("editorExit:function(form,item,value){if(isc.RPCManager.requestsArePending()){form._view._blurry=null;}else{var view=form._view;");
	}
	
	/**
	 * Closes blur/edit-exit handler bodies and resets async sequencing state.
	 *
	 * @param blurable The blurable.
	 * @param parentVisible The parent visible.
	 * @param parentEnabled The parent enabled.
	 */
	@Override
	public void visitedOnBlurEventHandler(Focusable blurable, boolean parentVisible, boolean parentEnabled) {
		code.append("}},");
		visitingAsync = false;
	}

	// Used to sort out server-side events into the bizEditedForServer() method.
	private boolean inOnAddedEventHandler = false;
	
	/**
	 * Starts a biz-added event handler body.
	 *
	 * @param addable The addable.
	 * @param parentVisible The parent visible.
	 * @param parentEnabled The parent enabled.
	 */
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
	
	/**
	 * Closes the biz-added event handler body.
	 *
	 * @param addable The addable.
	 * @param parentVisible The parent visible.
	 * @param parentEnabled The parent enabled.
	 */
	@Override
	public void visitedOnAddedEventHandler(Addable addable, boolean parentVisible, boolean parentEnabled) {
		code.append("},");
		inOnAddedEventHandler = false;
	}

	// Used to sort out server-side events into the bizEditedForServer() method.
	private boolean inOnEditedEventHandler = false;
	
	/**
	 * Starts a biz-edited event handler body.
	 *
	 * @param editable The editable.
	 * @param parentVisible The parent visible.
	 * @param parentEnabled The parent enabled.
	 */
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
	
	/**
	 * Closes the biz-edited event handler body.
	 *
	 * @param editable The editable.
	 * @param parentVisible The parent visible.
	 * @param parentEnabled The parent enabled.
	 */
	@Override
	public void visitedOnEditedEventHandler(Editable editable, boolean parentVisible, boolean parentEnabled) {
		code.append("},");
		inOnEditedEventHandler = false;
	}

	// Used to sort out server-side events into the bizEditedForServer() method.
	private boolean inOnRemovedEventHandler = false;
	
	/**
	 * Starts a biz-removed event handler body.
	 *
	 * @param removable The removable.
	 * @param parentVisible The parent visible.
	 * @param parentEnabled The parent enabled.
	 */
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
	
	/**
	 * Closes the biz-removed event handler body.
	 *
	 * @param removable The removable.
	 * @param parentVisible The parent visible.
	 * @param parentEnabled The parent enabled.
	 */
	@Override
	public void visitedOnRemovedEventHandler(Removable removable, boolean parentVisible, boolean parentEnabled) {
		code.append("},");
		inOnRemovedEventHandler = false;
	}
	
	/**
	 * Starts a biz-selected event handler body and enables async sequencing for list/data widgets.
	 *
	 * @param selectable The selectable.
	 * @param parentVisible The parent visible.
	 * @param parentEnabled The parent enabled.
	 */
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
	
	/**
	 * Closes the biz-selected event handler body and disables async sequencing.
	 *
	 * @param selectable The selectable.
	 * @param parentVisible The parent visible.
	 * @param parentEnabled The parent enabled.
	 */
	@Override
	public void visitedOnSelectedEventHandler(Selectable selectable, boolean parentVisible, boolean parentEnabled) {
		code.append("},");
		visitingAsync = false;
	}
	
	/**
	 * Starts a lookup picked-event handler body.
	 *
	 * @param lookup The lookup.
	 * @param parentVisible The parent visible.
	 * @param parentEnabled The parent enabled.
	 */
	@Override
	public void visitOnPickedEventHandler(LookupDescription lookup, boolean parentVisible, boolean parentEnabled) {
		code.append("bizPicked:function(form,item,value){var view=form._view;");
	}
	
	/**
	 * Closes the lookup picked-event handler body.
	 *
	 * @param lookup The lookup.
	 * @param parentVisible The parent visible.
	 * @param parentEnabled The parent enabled.
	 */
	@Override
	public void visitedOnPickedEventHandler(LookupDescription lookup, boolean parentVisible, boolean parentEnabled) {
		code.append("},");
	}
	
	/**
	 * Starts a lookup cleared-event handler body.
	 *
	 * @param lookup The lookup.
	 * @param parentVisible The parent visible.
	 * @param parentEnabled The parent enabled.
	 */
	@Override
	public void visitOnClearedEventHandler(LookupDescription lookup, boolean parentVisible, boolean parentEnabled) {
		code.append("bizCleared:function(form,item,value){var view=form._view;");
	}
	
	/**
	 * Closes the lookup cleared-event handler body.
	 *
	 * @param lookup The lookup.
	 * @param parentVisible The parent visible.
	 * @param parentEnabled The parent enabled.
	 */
	@Override
	public void visitedOnClearedEventHandler(LookupDescription lookup, boolean parentVisible, boolean parentEnabled) {
		code.append("},");
	}

	/**
	 * Emits JavaScript to trigger a rerender action for the specified source binding.
	 *
	 * @param rerender The rerender.
	 * @param source The source.
	 * @param parentVisible The parent visible.
	 * @param parentEnabled The parent enabled.
	 */
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

	/**
	 * Emits JavaScript to apply a disabled condition to a target binding.
	 *
	 * @param setDisabled The set disabled.
	 * @param parentVisible The parent visible.
	 * @param parentEnabled The parent enabled.
	 */
	@Override
	public void visitSetDisabledEventAction(SetDisabledEventAction setDisabled,
												boolean parentVisible,
												boolean parentEnabled) {
		code.append("view.setDisabled('").append(BindUtil.sanitiseBinding(setDisabled.getBinding()));
		code.append("','").append(setDisabled.getDisabledConditionName()).append("');");
	}

	/**
	 * Emits JavaScript to apply an invisible condition to a target binding.
	 *
	 * @param setInvisible The set invisible.
	 * @param parentVisible The parent visible.
	 * @param parentEnabled The parent enabled.
	 */
	@Override
	public void visitSetInvisibleEventAction(SetInvisibleEventAction setInvisible,
												boolean parentVisible,
												boolean parentEnabled) {
		code.append("view.setInvisible('").append(BindUtil.sanitiseBinding(setInvisible.getBinding()));
		code.append("','").append(setInvisible.getInvisibleConditionName()).append("');");
	}

	/**
	 * Emits JavaScript to toggle the disabled state of a target binding.
	 *
	 * @param toggleDisabled The toggle disabled.
	 * @param parentVisible The parent visible.
	 * @param parentEnabled The parent enabled.
	 */
	@Override
	public void visitToggleDisabledEventAction(ToggleDisabledEventAction toggleDisabled,
												boolean parentVisible,
												boolean parentEnabled) {
		code.append("view.toggleDisabled('").append(BindUtil.sanitiseBinding(toggleDisabled.getBinding()));
		code.append("');");
	}

	/**
	 * Emits JavaScript to toggle the visibility state of a target binding.
	 *
	 * @param toggleVisibility The toggle visibility.
	 * @param parentVisible The parent visible.
	 * @param parentEnabled The parent enabled.
	 */
	@Override
	public void visitToggleVisibilityEventAction(ToggleVisibilityEventAction toggleVisibility,
													boolean parentVisible,
													boolean parentEnabled) {
		code.append("view.toggleVisibility('").append(BindUtil.sanitiseBinding(toggleVisibility.getBinding()));
		code.append("');");
	}
	
	/**
	 * Visits an action parameter node.
	 *
	 * <p>Side effects: none; parameters are rendered in dedicated action writers.
	 *
	 * @param parameter The parameter.
	 * @param parentVisible The parent visible.
	 * @param parentEnabled The parent enabled.
	 */
	@Override
	public void visitParameter(Parameter parameter, boolean parentVisible, boolean parentEnabled) {
		// do nothing - parameters are handled separately
	}
	
	/**
	 * Visits a filter parameter node.
	 *
	 * <p>Side effects: none; filter parameters are rendered in dedicated list/filter writers.
	 *
	 * @param parameter The parameter.
	 * @param parentVisible The parent visible.
	 * @param parentEnabled The parent enabled.
	 */
	@Override
	public void visitFilterParameter(FilterParameter parameter, boolean parentVisible, boolean parentEnabled) {
		// do nothing - parameters are handled separately
	}

	/**
	 * This generates an ID based on the module name and document name and an incrementing number.
	 *
	 * @return A JavaScript expression that yields a unique component identifier.
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
	
	/**
	 * Appends width, height, and min/max constraints for a renderable component.
	 *
	 * @param sizable component metadata providing size and constraint hints
	 * @param defaultMinHeightInPixels fallback minimum height when no explicit minimum is set
	 * @param builder JavaScript buffer receiving size-related properties
	 */
	private void size(AbsoluteWidth sizable, 
						Integer defaultMinHeightInPixels,
						StringBuilder builder) {
		ShrinkWrap shrinkWrap = (sizable instanceof ShrinkWrapper shrinkWrapper) ? 
									shrinkWrapper.getShrinkWrap() :
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
				if (sizable instanceof RelativeWidth relative) {
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
		
		if (sizable instanceof AbsoluteSize absoluteSize) {
			if (ShrinkWrap.height.equals(shrinkWrap) || ShrinkWrap.both.equals(shrinkWrap)) {
				builder.append("height:1,");
				heightShrinkWrapped = true;
			}
			else {
				// NB Don't use height:'*' if there is no specified height because blurbs won't 
				// layout correctly based on their content.
				// Also, it doesn't help contentImages either to put in a '*'.
				Integer height = absoluteSize.getPixelHeight();
				if (height != null) {
					builder.append("height:").append(height).append(',');
				}
				else {
					if (sizable instanceof RelativeSize relativeSize) {
						height = relativeSize.getPercentageHeight();
						if (height != null) {
							builder.append("height:'").append(height).append("%',");
						}
					}
				}
			}
		}
		
		// process size constraints
		if (sizable instanceof MinimumHeight minimumHeight) {
			if (! heightShrinkWrapped) {
				Integer minHeight = minimumHeight.getMinPixelHeight();
				if (minHeight == null) {
					minHeight = defaultMinHeightInPixels;
				}
				if (minHeight != null) {
					builder.append("minHeight:").append(minHeight).append(',');
				}
			}
			if (sizable instanceof ConstrainableHeight constrainableHeight) {
				if (! heightShrinkWrapped) {
					Integer maxHeight = constrainableHeight.getMaxPixelHeight();
					if (maxHeight != null) {
						builder.append("maxHeight:").append(maxHeight).append(',');
					}
				}
				if (! widthShrinkWrapped) {
					if (sizable instanceof ConstrainableSize constrainable) {
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
	
	/**
	 * Appends disable-condition flags for add/zoom/edit/remove grid actions.
	 *
	 * @param grid grid metadata containing disable-condition bindings
	 * @param builder JavaScript buffer receiving action-disable properties
	 */
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

	/**
	 * Appends disable-condition flags for lookup pick/edit/add/clear controls.
	 *
	 * @param lookup lookup metadata containing disable-condition bindings
	 * @param builder JavaScript buffer receiving action-disable properties
	 */
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

	/**
	 * Applies bordered-group styling only when the component enables border rendering.
	 *
	 * @param title optional border title
	 * @param bordered border-capable metadata
	 * @param definedPixelPadding explicit padding value, or null
	 * @param builder JavaScript buffer receiving border properties
	 */
	private static void bordered(String title, Bordered bordered, Integer definedPixelPadding, StringBuilder builder) {
		if (Boolean.TRUE.equals(bordered.getBorder())) {
			border(title, definedPixelPadding, builder);
		}
	}

	/**
	 * Appends the base SmartClient group-border presentation properties.
	 *
	 * @param title optional border title
	 * @param definedPixelPadding explicit padding value, or null
	 * @param builder JavaScript buffer receiving border properties
	 */
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
	
	/**
	 * Appends a disabled-condition binding when configured.
	 *
	 * @param disabledConditionName condition binding name, or null
	 * @param builder JavaScript buffer receiving the property
	 */
	private static void disabled(String disabledConditionName, StringBuilder builder) {
		if (disabledConditionName != null) {
			builder.append("disabledConditionName:'").append(disabledConditionName).append("',");
		}
	}

	/**
	 * Appends an invisible-condition binding when configured.
	 *
	 * @param invisibleConditionName condition binding name, or null
	 * @param builder JavaScript buffer receiving the property
	 */
	private static void invisible(String invisibleConditionName, StringBuilder builder) {
		if (invisibleConditionName != null) {
			builder.append("invisibleConditionName:'").append(invisibleConditionName).append("',");
		}
	}

	/**
	 * Appends the effective editable state.
	 *
	 * @param editable metadata flag; null is treated as editable
	 * @param builder JavaScript buffer receiving the property
	 */
	private static void editable(Boolean editable, StringBuilder builder) {
		builder.append("editable:").append((! Boolean.FALSE.equals(editable)) ? "true," : "false,");
	}

	/**
	 * Removes a trailing comma from the JavaScript buffer, if present.
	 *
	 * @param builder JavaScript buffer to trim
	 */
	private static void removeTrailingComma(StringBuilder builder) {
		int length = builder.length();
		if (builder.charAt(length - 1) == ',') {
			builder.setLength(length - 1);
		}
	}

	/**
	 * Adds an action button to the generated action panel when it is panel-visible.
	 *
	 * @param actionName action name sent to the client runtime
	 * @param implicitName implicit action kind, or null
	 * @param displayName button display label
	 * @param inActionPanel whether this action should be rendered in the panel
	 * @param clientValidation whether client-side validation should run
	 * @param iconUrl optional icon URL
	 * @param iconStyleClass optional icon style class
	 * @param tooltip optional tooltip text
	 * @param confirmationText optional confirmation message
	 * @param parameters action parameters
	 * @param disabledConditionName optional disabled condition binding
	 * @param invisibleConditionName optional invisible condition binding
	 * @param canDelete delete permission used for remove actions
	 */
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

	/**
	 * Builds JavaScript for a zoom-in widget.
	 *
	 * @param label optional label text
	 * @param iconUrl optional icon URL
	 * @param iconStyleClass optional icon style class
	 * @param toolTip optional tooltip text
	 * @param zoomIn zoom-in metadata
	 * @return JavaScript constructor expression for the widget
	 */
	private String generateZoomIn(String label,
			String iconUrl,
			String iconStyleClass,
			String toolTip,
			ZoomIn zoomIn) {
		StringBuilder result = new StringBuilder(128);
		result.append("isc.BizZoomIn.create({");

		String binding = BindUtil.sanitiseBinding(zoomIn.getBinding());
		result.append("name:'")
				.append(binding)
				.append(".zoomIn',")
				.append("binding:'")
				.append(binding);

		if (label != null || iconStyleClass != null) {
			result.append("',displayName:'");

			if (iconStyleClass != null) {
				result.append("<i class=\"bizhubFontIcon ")
						.append(iconStyleClass)
						.append("\"></i>");
			}

			if (label != null) {
				result.append("<span> &nbsp;</span>")
						.append(OWASP.escapeJsString(label));
			}
		}

		result.append("',tabIndex:999,");

		if (iconStyleClass == null && iconUrl != null) {
			result.append("icon:'../")
					.append(iconUrl)
					.append("',");
		}

		size(zoomIn, null, result);
		disabled(zoomIn.getDisabledConditionName(), result);
		invisible(zoomIn.getInvisibleConditionName(), result);

		if (toolTip != null) {
			result.append("tooltip:'")
					.append(OWASP.escapeJsString(toolTip))
					.append("',");
		}

		result.append("_view:view})");

		return result.toString();
	}

	/**
	 * Builds JavaScript for an action button widget.
	 *
	 * @param actionName action name sent to the client runtime
	 * @param implicitName implicit action kind, or null
	 * @param label optional button label
	 * @param clientValidation whether client-side validation should run
	 * @param iconUrl optional icon URL
	 * @param iconStyleClass optional icon style class
	 * @param toolTip optional tooltip text
	 * @param confirmationText optional confirmation message
	 * @param parameters action parameters
	 * @param disabledConditionName optional disabled condition binding
	 * @param invisibleConditionName optional invisible condition binding
	 * @param button source button metadata when rendering declared buttons
	 * @param canDelete delete permission used for remove actions
	 * @return JavaScript constructor expression for the button
	 */
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
	
	/**
	 * Appends action parameters as JavaScript object properties.
	 *
	 * @param parameters parameter definitions to encode
	 * @param builder JavaScript buffer receiving encoded parameters
	 */
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

	/**
	 * Appends filter and named parameters for list/query requests.
	 *
	 * @param filterParameters filter parameter definitions
	 * @param parameters named parameter definitions
	 * @param builder JavaScript buffer receiving encoded request parameters
	 */
	private static void appendFilterParameters(@Nullable List<FilterParameter> filterParameters,
												@Nullable List<Parameter> parameters,
												@Nonnull StringBuilder builder) {
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
	
	/**
	 * Builds and appends a SmartClient field definition for the current form item.
	 *
	 * <p>Side effects: appends field JavaScript to the output buffer.
	 *
	 * @param widget input widget metadata
	 * @param typeOverride optional SmartClient type override
	 * @return prepared field definition
	 */
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
		def.setRequiredMessage(getCurrentWidgetRequiredMessage());
		String help = getCurrentWidgetHelp();
		if (help != null) {
			def.setHelpText(help);
		}
		
		code.append(def.toJavascript());
		code.append(',');

		return def;
	}
	
	/**
	 * Creates a query-column definition for SmartClient list rendering.
	 *
	 * @param user active user
	 * @param customer active customer metadata
	 * @param module module containing the query/document metadata
	 * @param document document owning the query column
	 * @param column query column metadata
	 * @param runtime whether runtime domain values should be resolved
	 * @param uxui active UX/UI profile name
	 * @return the generated query-column definition
	 */
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
	 * Returns the SmartClient field definition for a widget bound to the supplied document.
	 *
	 * @param document The document metadata containing the widget binding.
	 * @param widget The widget metadata to convert into a SmartClient field definition.
	 * @param runtime Whether the generated definition is for runtime execution.
	 * @return The SmartClient field definition for the widget.
	 */
	public SmartClientFieldDefinition getField(@SuppressWarnings("hiding") Document document, 
												InputWidget widget,
												boolean runtime) {
		return new SmartClientFieldDefinition(user, customer, module, document, widget, runtime, currentUxUi);
	}
	
	/**
	 * Creates a data-grid field definition for SmartClient list rendering.
	 *
	 * @param document document owning the widget
	 * @param widget widget metadata to convert into a field definition
	 * @param dataGridBinding binding of the owning data grid
	 * @param hasFormatter whether a formatter is already available
	 * @param runtime whether the generated definition is for runtime execution
	 * @return the generated data-grid field definition
	 */
	public SmartClientDataGridFieldDefinition getDataGridField(@SuppressWarnings("hiding") Document document,
    															InputWidget widget,
    															String dataGridBinding,
    															boolean hasFormatter,
    															boolean runtime) {
    	return new SmartClientDataGridFieldDefinition(user, customer, module, document, widget, dataGridBinding, hasFormatter, runtime, false, currentUxUi);
    }

    /**
	* Appends a SmartClient data source definition for a document list model.
	*
	* <p>Side effects: appends generated JavaScript to the supplied buffer and tracks
	* visited data-source names to prevent recursive duplication.
	*
	* @param user active user
	* @param customer active customer metadata
	* @param owningModule module that owns the model
	* @param owningDocument document that owns the model
	* @param modelName list model name
	* @param uxui active UX/UI profile name
	* @param config whether to generate configuration-only output
	* @param toAppendTo buffer receiving the generated definition
	* @param visitedQueryNames visited data-source names used to break recursion
	* @return the generated data-source id
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
			throw new MetaDataException("List Model " + model + " has no driving document defined and smart client does not support dynamic/late list grid generation");
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
	* Appends a SmartClient data source definition for a module query.
	*
	* <p>Side effects: appends generated JavaScript to the supplied buffer and tracks
	* visited data-source names to prevent recursive duplication.
	*
	* @param user active user
	* @param customer active customer metadata
	* @param query query metadata to render
	* @param dataSourceIDOverride optional forced data-source id
	* @param forLookup lookup metadata that may contribute hidden bindings
	* @param uxui active UX/UI profile name
	* @param config whether to generate configuration-only output
	* @param toAppendTo buffer receiving the generated definition
	* @param visitedQueryNames visited data-source names used to break recursion
	* @return the generated data-source id
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
	
	/**
	 * Appends a full SmartClient data source definition and any nested lookup data sources.
	 *
	 * @param user active user
	 * @param customer active customer metadata
	 * @param owningModuleName module owning the model/query
	 * @param owningDocument owning document metadata
	 * @param drivingDocumentModule module of the driving document
	 * @param drivingDocument driving document metadata
	 * @param queryName query name, or null
	 * @param aggregateQuery whether the query is aggregate
	 * @param modelName model name, or null
	 * @param description localized data-source title
	 * @param columns query columns to encode
	 * @param dataSourceIDOverride explicit data-source id override, or null
	 * @param forLookup lookup context when generating nested definitions
	 * @param uxui active UX/UI profile name
	 * @param config true to generate configuration-only output
	 * @param toAppendTo JavaScript buffer receiving generated definitions
	 * @param visitedQueryNames visited data-source ids used to avoid recursion loops
	 * @return generated data-source id
	 */
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
			toAppendTo.append("',compareCriteria:function(newCriteria,oldCriteria,requestProperties,policy){if(this._drop){this.invalidateCache(true);return -1;}else{return this.Super('compareCriteria',arguments)}}");
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
			toAppendTo.append(OWASP.escapeJsString(Util.nullSafeI18n("ui.tag"), false, true));
			toAppendTo.append("',type:'boolean',validOperators:['equals']},");
			toAppendTo.append("{name:'bizFlagComment',title:'");
			toAppendTo.append(OWASP.escapeJsString(Util.nullSafeI18n("ui.flag"), false, true));
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
			if ((column instanceof MetaDataQueryProjectedColumn projectedColumn) && 
					(! projectedColumn.isProjected())) {
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
	
	/**
	 * Validates that collapsible containers declare a border title.
	 *
	 * @param collapsible collapsible mode, or null
	 * @param borderTitle localized border title, or null
	 * @throws MetaDataException if collapsible is set without a border title
	 */
	private static void validateCollapsible(Collapsible collapsible,String borderTitle) {
		if (collapsible != null && borderTitle == null) {
			throw new MetaDataException("Border title must be defined if the collapsible attribute is present");
		}
	}

	/**
	 * Renders a SmartClient sidebar container into the current view output.
	 *
	 * @param sidebar sidebar metadata to render
	 */
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
	
	/**
	 * Closes the current SmartClient sidebar container.
	 *
	 * @param sidebar sidebar metadata that was just rendered
	 */
	@Override
	public void renderedSidebar(Sidebar sidebar) {
		containerVariables.pop();
	}
}
