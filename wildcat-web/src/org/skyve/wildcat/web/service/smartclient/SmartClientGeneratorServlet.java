package org.skyve.wildcat.web.service.smartclient;

import java.io.IOException;
import java.io.PrintWriter;
import java.security.Principal;
import java.util.List;
import java.util.Stack;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.skyve.content.MimeType;
import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;
import org.skyve.domain.messages.MessageException;
import org.skyve.domain.messages.SessionEndedException;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Collection;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.DynamicImage.ImageFormat;
import org.skyve.metadata.model.document.Relation;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.Query;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.Action;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.metadata.view.widget.bound.FilterParameter;
import org.skyve.metadata.view.widget.bound.Parameter;
import org.skyve.util.Binder.TargetMetaData;
import org.skyve.wildcat.bind.BindUtil;
import org.skyve.wildcat.generate.SmartClientGenerateUtils;
import org.skyve.wildcat.generate.SmartClientGenerateUtils.SmartClientDataGridFieldDefinition;
import org.skyve.wildcat.generate.SmartClientGenerateUtils.SmartClientFieldDefinition;
import org.skyve.wildcat.generate.SmartClientGenerateUtils.SmartClientLookupDefinition;
import org.skyve.wildcat.metadata.customer.CustomerImpl;
import org.skyve.wildcat.metadata.model.document.DocumentImpl;
import org.skyve.wildcat.metadata.module.ModuleImpl;
import org.skyve.wildcat.metadata.view.AbsoluteSize;
import org.skyve.wildcat.metadata.view.AbsoluteWidth;
import org.skyve.wildcat.metadata.view.ActionImpl;
import org.skyve.wildcat.metadata.view.Bordered;
import org.skyve.wildcat.metadata.view.ContentSpecifiedWidth;
import org.skyve.wildcat.metadata.view.HorizontalAlignment;
import org.skyve.wildcat.metadata.view.Inject;
import org.skyve.wildcat.metadata.view.RelativeSize;
import org.skyve.wildcat.metadata.view.ViewImpl;
import org.skyve.wildcat.metadata.view.ViewVisitor;
import org.skyve.wildcat.metadata.view.container.Box;
import org.skyve.wildcat.metadata.view.container.HBox;
import org.skyve.wildcat.metadata.view.container.Tab;
import org.skyve.wildcat.metadata.view.container.TabPane;
import org.skyve.wildcat.metadata.view.container.VBox;
import org.skyve.wildcat.metadata.view.container.form.Form;
import org.skyve.wildcat.metadata.view.container.form.FormColumn;
import org.skyve.wildcat.metadata.view.container.form.FormItem;
import org.skyve.wildcat.metadata.view.container.form.FormRow;
import org.skyve.wildcat.metadata.view.event.Changeable;
import org.skyve.wildcat.metadata.view.event.Focusable;
import org.skyve.wildcat.metadata.view.event.RerenderEventAction;
import org.skyve.wildcat.metadata.view.event.ServerSideActionEventAction;
import org.skyve.wildcat.metadata.view.event.SetDisabledEventAction;
import org.skyve.wildcat.metadata.view.event.SetInvisibleEventAction;
import org.skyve.wildcat.metadata.view.event.ToggleDisabledEventAction;
import org.skyve.wildcat.metadata.view.widget.Blurb;
import org.skyve.wildcat.metadata.view.widget.Button;
import org.skyve.wildcat.metadata.view.widget.DialogButton;
import org.skyve.wildcat.metadata.view.widget.DynamicImage;
import org.skyve.wildcat.metadata.view.widget.GeoLocator;
import org.skyve.wildcat.metadata.view.widget.Link;
import org.skyve.wildcat.metadata.view.widget.MapDisplay;
import org.skyve.wildcat.metadata.view.widget.Spacer;
import org.skyve.wildcat.metadata.view.widget.StaticImage;
import org.skyve.wildcat.metadata.view.widget.bound.Label;
import org.skyve.wildcat.metadata.view.widget.bound.ProgressBar;
import org.skyve.wildcat.metadata.view.widget.bound.input.CheckBox;
import org.skyve.wildcat.metadata.view.widget.bound.input.CheckMembership;
import org.skyve.wildcat.metadata.view.widget.bound.input.ColourPicker;
import org.skyve.wildcat.metadata.view.widget.bound.input.Combo;
import org.skyve.wildcat.metadata.view.widget.bound.input.Comparison;
import org.skyve.wildcat.metadata.view.widget.bound.input.ContentImage;
import org.skyve.wildcat.metadata.view.widget.bound.input.ContentLink;
import org.skyve.wildcat.metadata.view.widget.bound.input.Geometry;
import org.skyve.wildcat.metadata.view.widget.bound.input.HTML;
import org.skyve.wildcat.metadata.view.widget.bound.input.InputWidget;
import org.skyve.wildcat.metadata.view.widget.bound.input.ListMembership;
import org.skyve.wildcat.metadata.view.widget.bound.input.Lookup;
import org.skyve.wildcat.metadata.view.widget.bound.input.LookupDescription;
import org.skyve.wildcat.metadata.view.widget.bound.input.Password;
import org.skyve.wildcat.metadata.view.widget.bound.input.Radio;
import org.skyve.wildcat.metadata.view.widget.bound.input.RichText;
import org.skyve.wildcat.metadata.view.widget.bound.input.Slider;
import org.skyve.wildcat.metadata.view.widget.bound.input.Spinner;
import org.skyve.wildcat.metadata.view.widget.bound.input.TextArea;
import org.skyve.wildcat.metadata.view.widget.bound.input.TextField;
import org.skyve.wildcat.metadata.view.widget.bound.tabular.DataGrid;
import org.skyve.wildcat.metadata.view.widget.bound.tabular.DataGridBoundColumn;
import org.skyve.wildcat.metadata.view.widget.bound.tabular.DataGridContainerColumn;
import org.skyve.wildcat.metadata.view.widget.bound.tabular.DisableableCRUDGrid;
import org.skyve.wildcat.metadata.view.widget.bound.tabular.ListGrid;
import org.skyve.wildcat.metadata.view.widget.bound.tabular.PickList;
import org.skyve.wildcat.metadata.view.widget.bound.tabular.PickListColumn;
import org.skyve.wildcat.persistence.AbstractPersistence;
import org.skyve.wildcat.util.UtilImpl;
import org.skyve.wildcat.web.AbstractWebContext;
import org.skyve.wildcat.web.ServletConstants;
import org.skyve.wildcat.web.WebUtil;

/**
 * Generates views based on bizhub's XML view spec.
 */
public class SmartClientGeneratorServlet extends HttpServlet {
	private static final long serialVersionUID = 1L;

	private static final String UX_UI = "desktop";
	
	private static class SmartClientViewVisitor extends ViewVisitor {
		private User user;
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
			else if (ViewType.edit.equals(view.getType())) {
				code.append("var edit = BizContainer.create({width:'100%',height:'100%',invisibleConditionName:'");
				code.append(Bean.NOT_CREATED_KEY);
				code.append("'});");
				containerVariables.push("edit");
			}
			else if (ViewType.create.equals(view.getType())) {
				code.append("var create = BizContainer.create({width:'100%',height:'100%',invisibleConditionName:'");
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
			code.append("var ").append(variable).append("=BizTabPane.create({");
			size(tabPane, code);
			disabled(tabPane.getDisabledConditionName(), code);
			invisible(tabPane.getInvisibleConditionName(), code);
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
			code.append("var ").append(variable).append("=BizContainer.create({});\n");

			containerVariables.push(variable);
		}

		@Override
		public void visitedTab(Tab tab, 
								boolean parentVisible,
								boolean parentEnabled) {
			String paneVariable = containerVariables.pop();
			String tabPaneVariable = containerVariables.peek();
			Integer tabNumber = tabNumbers.pop();
			code.append(tabPaneVariable).append(".addBizTab({name:'").append(tabNumber).append("',title:'");
			code.append(SmartClientGenerateUtils.processString(tab.getTitle())).append("',pane:").append(paneVariable).append(',');
			tabNumbers.push(Integer.valueOf(tabNumber.intValue() + 1));
			disabled(tab.getDisabledConditionName(), code);
			invisible(tab.getInvisibleConditionName(), code);
			String selected = tab.getSelectedConditionName();
			if (selected != null) {
				code.append("selectedConditionName:'").append(selected).append("',");
			}
			removeTrailingComma(code);
			code.append("});\n");
		}

		@Override
		public void visitVBox(VBox vbox, 
								boolean parentVisible,
								boolean parentEnabled) {
			String variable = "v" + variableCounter++;
			code.append("var ").append(variable).append("=BizVBox.create({");
			size(vbox, code);
			box(vbox);
			bordered(vbox, code);
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
			code.append("var ").append(variable).append("=BizHBox.create({");
			size(hbox, code);
			box(hbox);
			bordered(hbox, code);
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
		}

		private boolean viewHasAtLeastOneForm = false;
		private String formVariable = null;
		private VBox borderBox = null;
		
		@Override
		public void visitForm(Form form, 
								boolean parentVisible,
								boolean parentEnabled)
		throws MetaDataException {
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
				borderBox.setPercentageHeight(form.getPercentageHeight());
				borderBox.setPercentageWidth(form.getPercentageWidth());
				borderBox.setPixelHeight(form.getPixelHeight());
				borderBox.setPixelWidth(form.getPixelWidth());
				borderBox.setPixelPadding(Integer.valueOf(5));
				visitVBox(borderBox, parentVisible, parentEnabled);
			}
			
			formVariable = "v" + variableCounter++;
			code.append("var ").append(formVariable);
			code.append("=DynamicForm.create({longTextEditorType:'text',longTextEditorThreshold:102400,");
			// SC docs says that autoFocus will focus in first focusable item
			// in the form when it is drawn.
			// Don't use autoFocus as we have multiple dynamic forms that can be declared
			// in some views which doesn't work.
			code.append("_view:view,");
			code.append("ID:").append(IDExpression()).append(',');
			disabled(form.getDisabledConditionName(), code);
//code.append("cellBorder:1,");
			
			if (! Boolean.TRUE.equals(border)) { // false or null
				size(form, code);
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
									boolean parentEnabled)
		throws MetaDataException {
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
										boolean parentEnabled)
		throws MetaDataException {
			Integer percentage = column.getPercentageWidth();
			Integer pixel = column.getPixelWidth();
			if (percentage != null) {
				code.append('\'').append(percentage).append("%',");
			}
			else if (pixel != null) {
				code.append(pixel).append(',');
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
									boolean parentEnabled)
		throws MetaDataException {
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
		public void visitFormItem(FormItem item, boolean parentVisible, boolean parentEnabled)
		throws MetaDataException {
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
//			item.getVerticalAlignment()
//			item.getShowHelp()
//			code.append("{width:'*',");
		}

		@Override
		public void visitedFormItem(FormItem item,
										boolean parentVisible,
										boolean parentEnabled)
		throws MetaDataException {
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
									boolean parentEnabled)
		throws MetaDataException {
			// nothing to do here
		}

		@Override
		public void visitButton(Button button,
									boolean parentVisible,
									boolean parentEnabled) {
			Action action = view.getAction(button.getActionName());

			String buttonCode = generateButton(action.getName(), 
												action.getImplicitName(), 
												action.getDisplayName(),
												action.getClientValidation(),
												action.getRelativeIconFileName(),
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
					code.append("type:'canvas',showTitle:false,width:1,canvas:HLayout.create({height:22,members:[");
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
			geoLocatorCode.append("BizUtil.createGeoLocator(view,");
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
				code.append("type:'canvas',showTitle:false,canvas:HLayout.create({height:22,members:[");
				code.append(geoLocatorCode).append(")]}),");
			}
		}

		@Override
		public void visitGeometry(Geometry geometry, 
									boolean parentVisible,
									boolean parentEnabled)
		throws MetaDataException {
			if (dataGridVariable != null) {
				dataGridColumnInputWidget = geometry;
				return;
			}

			if (formVariable == null) {
				throw new MetaDataException("Geometry found without a form");
			}

			preProcessFormItem(geometry, "geometry");
			size(geometry, code);
			disabled(geometry.getDisabledConditionName(), code);
			invisible(geometry.getInvisibleConditionName(), code);
			
			// Highlight text on focus
			code.append("selectOnFocus:true,");
			
			// TODO add in the filter operators allowed
		}

		@Override
		public void visitMap(MapDisplay map,
								boolean parentVisible,
								boolean parentEnabled)
		throws MetaDataException {
			String variable = "v" + variableCounter++;
			code.append("var ").append(variable).append("=BizMap.create({_view:view});");
    		code.append(variable).append(".setDataSource('").append(map.getModelName()).append("');\n");
			code.append(containerVariables.peek()).append(".addContained(").append(variable).append(");\n");
/*
private void addImage(DynamicImage image) {
code.append("BizDynamicImage.create({name:'");
code.append(image.getName());
code.append("',moduleDotDocument:'");
code.append(document.getOwningModuleName());
code.append('.');
code.append(document.getName()).append("',");
size(image, code);
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
*/
		}

		@Override
		public void visitDialogButton(DialogButton button,
										boolean parentVisible,
										boolean parentEnabled) {
			if (formVariable == null) {
				String variable = "v" + variableCounter++;
				code.append("var ").append(variable).append("=BizLabel.create({value: '");
				code.append(SmartClientGenerateUtils.processString(button.getCommand()));
				code.append("'});\n");
				code.append(containerVariables.peek()).append(".addContained(").append(variable).append(");\n");
			}
			else {
				code.append("type:'blurb',defaultValue:'dialog button ");
				code.append(SmartClientGenerateUtils.processString(button.getCommand())).append("',");
				disabled(button.getDisabledConditionName(), code);
				invisible(button.getInvisibleConditionName(), code);
			}
		}

		@Override
		public void visitDynamicImage(DynamicImage image,
										boolean parentVisible,
										boolean parentEnabled)
		throws MetaDataException {
			// markup is generated in the JSON data for a data grid container column dynamic image
			if (dataGridVariable != null) {
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

		private void addImage(DynamicImage image) throws MetaDataException {
			code.append("BizDynamicImage.create({name:'");
			code.append(image.getName());
			code.append("',moduleDotDocument:'");
			code.append(document.getOwningModuleName());
			code.append('.');
			code.append(document.getName()).append("',format:'");
			ImageFormat format = document.getDynamicImage(customer, image.getName()).getFormat();
			code.append((format != null) ? format : ImageFormat.png).append("',");
			size(image, code);
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
			if (dataGridVariable != null) {
				return;
			}

			if (formVariable == null) {
				String variable = "v" + variableCounter++;
				code.append("var ").append(variable).append('=');
				addStaticImage(image);
				code.append(containerVariables.peek()).append(".addContained(").append(variable).append(");\n");
			}
			else {
				code.append("type:'canvas',showTitle:false,canvas:");
				addStaticImage(image);
				code.append(',');
			}
		}

		private void addStaticImage(StaticImage image) {
			code.append("BizImage.create({file: '").append(image.getRelativeFile()).append('\'');
			size(image, code);
			code.append("});\n");
		}

		@Override
		public void visitSpacer(Spacer spacer) throws MetaDataException {
			code.append("type:'spacer',");
		}

		@Override
		public void visitLink(Link link, 
								boolean parentVisible, 
								boolean parentEnabled)
		throws MetaDataException {
			// markup is generated in the JSON data for a data grid container column link
			if (dataGridVariable != null) {
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
				else {
					label = SmartClientGenerateUtils.processString(label);
				}
				code.append("title:'").append(label).append("',");
				code.append("type:'blurb',name:'_");
				code.append(formatCounter++).append("',"); // _1, _2 and so on
				size(link, code);
				invisible(link.getInvisibleConditionName(), code);
			}
		}

		@Override
		public void visitBlurb(Blurb blurb,
								boolean parentVisible,
								boolean parentEnabled)
		throws MetaDataException {
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
								boolean parentEnabled)
		throws MetaDataException {
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
				if ((dataGridBinding == null) && (formVariable == null)) {
					throw new MetaDataException("Label or blurb with a value of [" + value + 
													"] contains a binding expression and must be declared within a form element or a data grid container column to be able to bind correctly");
				}

				value = null;
				binding = "_" + formatCounter++; // _1, _2 and so on
			}
			
			// markup is generated in the JSON data for a data grid container column label or a dynamic form-based value
			if (dataGridVariable != null) {
				return;
			}
			
			HorizontalAlignment alignment = label.getTextAlignment();

			if (formVariable == null) {
				String variable = "v" + variableCounter++;
				code.append("var ").append(variable).append("=BizLabel.create({");

		        size(label, code);
		        
				if (alignment != null) {
					code.append("textAlign:'").append(alignment.toAlignmentString()).append("',");
				}
				
				if (binding == null) {
					code.append("value:'");
					code.append(SmartClientGenerateUtils.processString((value == null) ? displayName : value, false, false));
				}
				else {
					code.append("binding:'").append(binding.replace('.', '_'));
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
				title = SmartClientGenerateUtils.processString(title);
				code.append("endRow:false,title:'").append(title).append("',type:'blurb',");
				if (binding == null) {
					code.append("defaultValue:'").append(SmartClientGenerateUtils.processString((value == null) ? displayName : value, false, false));
				}
				else {
					code.append("name:'").append(binding.replace('.', '_'));
				}
				code.append("',");
				
				if (alignment != null) {
					code.append("textAlign:'").append(alignment.toAlignmentString()).append("',");
				}
				size(label, code);
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
											boolean parentEnabled)
		throws MetaDataException {
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
			code.append("var ").append(variable).append("=BizLabel.create({value: '");
			code.append(progressBar.getBinding());
			code.append("'});\n");
			code.append(containerVariables.peek()).append(".addContained(").append(variable).append(");\n");
		}

		private String dataGridVariable = null;
		private Document dataGridDocument = null;
		private String dataGridBinding = null;

		@Override
		public void visitDataGrid(DataGrid grid,
									boolean parentVisible,
									boolean parentEnabled)
		throws MetaDataException {
			dataGridBinding = grid.getBinding();
			TargetMetaData target = BindUtil.getMetaDataForBinding(customer,
																	module,
																	document,
																	dataGridBinding);
			Relation relation = (Relation) target.getAttribute();
			String documentName = relation.getDocumentName();

			dataGridDocument = module.getDocument(customer, documentName);
			dataGridVariable = "v" + variableCounter++;
			code.append("var ").append(dataGridVariable).append("=BizDataGrid.create({_mod:'");
			code.append(dataGridDocument.getOwningModuleName());
			code.append("',_doc:'");
			code.append(dataGridDocument.getName());
			code.append("',_b:'").append(dataGridBinding.replace('.', '_'));
			code.append("',ID:").append(IDExpression());
			code.append(",canCreate:").append(user.canCreateDocument(dataGridDocument));
			code.append(",canUpdate:").append(user.canUpdateDocument(dataGridDocument));
			code.append(",canDelete:").append(user.canDeleteDocument(dataGridDocument)).append(',');
			disableCRUD(grid, code);
			if (Boolean.TRUE.equals(grid.getInline())) { // defaults to not being inline
				code.append("inline:true,");
			}
			if (Boolean.TRUE.equals(grid.getWordWrap())) { // defaults to not being wrapped
				code.append("wordWrap:true,");
			}
			if ((relation instanceof Collection) && 
					Boolean.TRUE.equals(((Collection) relation).getOrdered())) {
				code.append("_ordinal:'").append(ChildBean.ORDINAL_KEY).append("',");
			}
			String title = grid.getTitle();
			if (title != null) {
				code.append("title:'");
				code.append(SmartClientGenerateUtils.processString(grid.getTitle())).append("',");
			}
			size(grid, code);
			disabled(grid.getDisabledConditionName(), code);
			invisible(grid.getInvisibleConditionName(), code);
			editable(grid.getEditable(), code);
			code.append("_fields:[");
		}

		@Override
		public void visitedDataGrid(DataGrid grid,
										boolean parentVisible,
										boolean parentEnabled) {
			code.setLength(code.length() - 1); // remove trailing comma from list grid field definition
			code.append("],_view:view});\n");
			code.append(containerVariables.peek()).append(".addContained(").append(dataGridVariable).append(");\n");
			dataGridVariable = null;
			dataGridDocument = null;
			dataGridBinding = null;
		}

		private InputWidget dataGridColumnInputWidget;
		
		@Override
		public void visitDataGridBoundColumn(DataGridBoundColumn column,
												boolean parentVisible,
												boolean parentEnabled)
		throws MetaDataException {
			// do nothing
		}

		@Override
		public void visitedDataGridBoundColumn(DataGridBoundColumn column,
												boolean parentVisible,
												boolean parentEnabled)
		throws MetaDataException {
			if (dataGridColumnInputWidget != null) {
				SmartClientDataGridFieldDefinition def = null;
				String binding = column.getBinding();
				if (binding == null) { // column bound to collection for the grid
					def = SmartClientGenerateUtils.getDataGridField(user,
																		customer,
																		module, 
																		document, 
																		dataGridColumnInputWidget, 
																		dataGridBinding);
				} 
				else {
					def = SmartClientGenerateUtils.getDataGridField(user,
																		customer,
																		module, 
																		dataGridDocument, 
																		dataGridColumnInputWidget, 
																		null);
				}

				String title = column.getTitle();
				if (title != null) {
					def.setTitle(title);
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
																			(Lookup) dataGridColumnInputWidget, 
																			false,
																			ds,
																			null);
					code.insert(0, ds);
				}
				dataGridColumnInputWidget = null;
			}
		}

		@Override
		public void visitDataGridContainerColumn(DataGridContainerColumn column,
													boolean parentVisible,
													boolean parentEnabled)
		throws MetaDataException {
			code.append("{name:'_").append(formatCounter++);
			code.append("',type:'text',formatCellValue:'value;',canEdit:false,title:'");
			
			String title = column.getTitle();
			code.append((title == null) ? " " : title).append('\'');
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
													boolean parentEnabled)
		throws MetaDataException {
			// do nothing
		}
		
		private String listGridVariable = null;

		@Override
		public void visitListGrid(ListGrid grid,
									boolean parentVisible,
									boolean parentEnabled)
		throws MetaDataException {
			Query query = module.getQuery(grid.getQueryName());
			StringBuilder ds = new StringBuilder(256);
			String dataSourceId = SmartClientGenerateUtils.appendDataSourceDefinition(user, customer, query, null, null, false, ds, null);
			code.insert(0, ds);

			listGridVariable = "v" + variableCounter++;
			code.append("var ").append(listGridVariable).append("=BizListGrid.create({");
			code.append("ID:").append(IDExpression()).append(',');
			String title = grid.getTitle();
			if (title != null) {
				code.append("title:'");
				code.append(SmartClientGenerateUtils.processString(grid.getTitle())).append("',");
			}
			code.append("dataSource:'").append(dataSourceId).append("',");
			code.append("name:'").append(listGridVariable).append("',");
			code.append("contConv:").append(grid.getContinueConversation()).append(",");
			disabled(grid.getDisabledConditionName(), code);
			invisible(grid.getInvisibleConditionName(), code);
			disableCRUD(grid, code);
		}

		@Override
		public void visitedListGrid(ListGrid grid,
										boolean parentVisible,
										boolean parentEnabled) {
			appendFilterParameters(grid.getParameters(), code);
			code.append("_view:view});\n");
			code.append(containerVariables.peek()).append(".addContained(").append(listGridVariable).append(");\n");
			listGridVariable = null;
		}

		@Override
		public void visitPickList(PickList list,
									boolean parentVisible,
									boolean parentEnabled) {
			String variable = "v" + variableCounter++;
			code.append("var ").append(variable).append("=BizLabel.create({value: '");
			code.append(SmartClientGenerateUtils.processString(list.getTitle()));
			code.append("'});\n");
			code.append(containerVariables.peek()).append(".addContained(").append(variable).append(");\n");
		}

		@Override
		public void visitedPickList(PickList list,
										boolean parentVisible,
										boolean parentEnabled) {
			// do nothing
		}

		@Override
		public void visitPickListColumn(PickListColumn column,
											boolean parentVisible,
											boolean parentEnabled) {
			String variable = "v" + variableCounter++;
			code.append("var ").append(variable).append("=BizLabel.create({value: '");
			code.append(SmartClientGenerateUtils.processString(column.getTitle()));
			code.append("'});\n");
			code.append(containerVariables.peek()).append(".addContained(").append(variable).append(");\n");
		}

		@Override
		public void visitCheckBox(CheckBox checkBox,
									boolean parentVisible,
									boolean parentEnabled)
		throws MetaDataException {
			if (dataGridVariable != null) {
				dataGridColumnInputWidget = checkBox;
				return;
			}
			
			preProcessFormItem(checkBox, "checkbox");
			size(checkBox, code);
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
										boolean parentEnabled)
		throws MetaDataException {
			// do nothing
		}

		// TODO implement this - does this need size? probably
		@Override
		public void visitCheckMembership(CheckMembership membership,
											boolean parentVisible,
											boolean parentEnabled) {
			String variable = "v" + variableCounter++;
			code.append("var ").append(variable).append("=BizLabel.create({value: '");
			code.append("check membership").append(membership.getBinding());
			code.append("'});\n");
			code.append(containerVariables.peek()).append(".addContained(").append(variable).append(");\n");
		}

		@Override
		public void visitedCheckMembership(CheckMembership membership,
											boolean parentVisible, 
											boolean parentEnabled)
		throws MetaDataException {
			// do nothing - until implemented properly
		}

		@Override
		public void visitColourPicker(ColourPicker colour,
										boolean parentVisible,
										boolean parentEnabled)
		throws MetaDataException {
			if (dataGridVariable != null) {
				dataGridColumnInputWidget = colour;
				return;
			}

			if (formVariable == null) {
				throw new MetaDataException("Colour found without a form");
			}

			preProcessFormItem(colour, "color");
			size(colour, code);
			disabled(colour.getDisabledConditionName(), code);
			invisible(colour.getInvisibleConditionName(), code);
		}

		@Override
		public void visitedColourPicker(ColourPicker colour,
											boolean parentVisible,
											boolean parentEnabled)
		throws MetaDataException {
			// do nothing
		}

		@Override
		public void visitCombo(Combo combo,
								boolean parentVisible,
								boolean parentEnabled)
		throws MetaDataException {
			if (dataGridVariable != null) {
				dataGridColumnInputWidget = combo;
				return;
			}

			if (formVariable == null) {
				throw new MetaDataException("Combo found without a form");
			}

			preProcessFormItem(combo, "select");
			size(combo, code);
			disabled(combo.getDisabledConditionName(), code);
			invisible(combo.getInvisibleConditionName(), code);
		}

		@Override
		public void visitedCombo(Combo combo,
									boolean parentVisible,
									boolean parentEnabled)
		throws MetaDataException {
			// do nothing
		}

		@Override
		public void visitContentImage(ContentImage image,
										boolean parentVisible, 
										boolean parentEnabled)
		throws MetaDataException {
			// markup is generated in the JSON data for a data grid container column content image
			if (dataGridVariable != null) {
				dataGridColumnInputWidget = image;
				return;
			}

			if (formVariable == null) {
				throw new MetaDataException("ContentImage found without a form");
			}

			preProcessFormItem(image, "bizContentImage");
			size(image, code);
			disabled(image.getDisabledConditionName(), code);
			invisible(image.getInvisibleConditionName(), code);
			editable(image.getEditable(), code);
		}

		@Override
		public void visitContentLink(ContentLink link,
										boolean parentVisible,
										boolean parentEnabled)
		throws MetaDataException {
			if (dataGridVariable != null) {
				dataGridColumnInputWidget = link;
				return;
			}

			if (formVariable == null) {
				throw new MetaDataException("ContentLink found without a form");
			}

			preProcessFormItem(link, "bizContentLink");
			disabled(link.getDisabledConditionName(), code);
			invisible(link.getInvisibleConditionName(), code);
			editable(link.getEditable(), code);
		}

		@Override
		public void visitRichText(RichText text,
									boolean parentVisible,
									boolean parentEnabled)
		throws MetaDataException {
			if (dataGridVariable != null) {
				dataGridColumnInputWidget = text;
				return;
			}

			if (formVariable == null) {
				throw new MetaDataException("RichText found without a form");
			}

			preProcessFormItem(text, "richText");
			size(text, code);
			disabled(text.getDisabledConditionName(), code);
			invisible(text.getInvisibleConditionName(), code);
		}

		@Override
		public void visitedRichText(RichText richText,
										boolean parentVisible,
										boolean parentEnabled)
		throws MetaDataException {
			// do nothing
		}

		@Override
		public void visitHTML(HTML html,
								boolean parentVisible,
								boolean parentEnabled)
		throws MetaDataException {
			if (dataGridVariable != null) {
				dataGridColumnInputWidget = html;
				return;
			}

			if (formVariable == null) {
				throw new MetaDataException("HTML found without a form");
			}

			preProcessFormItem(html, "bizHTML");
			size(html, code);
			disabled(html.getDisabledConditionName(), code);
			invisible(html.getInvisibleConditionName(), code);
		}

		// indicates if we are visiting a list member widget.
		// this allow specific javascript for events to be generated since this widget lives outside of a form
		private boolean visitingListMembership = false;
		
		@Override
		public void visitListMembership(ListMembership membership,
											boolean parentVisible,
											boolean parentEnabled)
		throws MetaDataException {
			visitingListMembership = true;
			
			String membershipBinding = membership.getBinding();
			TargetMetaData target = BindUtil.getMetaDataForBinding(customer,
																	module,
																	document,
																	membershipBinding);
			Relation relation = (Relation) target.getAttribute();

			String variable = "v" + variableCounter++;
			code.append("var ").append(variable).append("=BizListMembership.create({_b:'");
			code.append(membershipBinding.replace('.', '_'));
			code.append('\'');
			String heading = membership.getCandidatesHeading();
			if (heading != null) {
				code.append(",candidatesHeading:'");
				code.append(SmartClientGenerateUtils.processString(heading)).append('\'');
			}
			heading = membership.getMembersHeading();
			if (heading != null) {
				code.append(",membersHeading:'");
				code.append(SmartClientGenerateUtils.processString(heading)).append('\'');
			}
			if ((relation instanceof Collection) && 
					Boolean.TRUE.equals(((Collection) relation).getOrdered())) {
				code.append(",_ordinal:'").append(ChildBean.ORDINAL_KEY).append('\'');
			}
			code.append(",_view:view,");
			disabled(membership.getDisabledConditionName(), code);
			invisible(membership.getInvisibleConditionName(), code);
			
			containerVariables.push(variable);
		}
		
		@Override
		public void visitedListMembership(ListMembership membership,
											boolean parentVisible,
											boolean parentEnabled)
		throws MetaDataException {
			removeTrailingComma(code);
			code.append("});\n");

			String variable = containerVariables.pop();
			code.append(containerVariables.peek()).append(".addContained(").append(variable).append(");\n");

			visitingListMembership = false;
		}

		@Override
		public void visitComparison(Comparison comparison,
										boolean parentVisible,
										boolean parentEnabled) {
			String variable = "v" + variableCounter++;
			code.append("var ").append(variable).append("=BizComparison.create({_b:'");
			code.append(comparison.getBinding().replace('.', '_'));
			code.append("',_view:view,");
			disabled(comparison.getDisabledConditionName(), code);
			invisible(comparison.getInvisibleConditionName(), code);
			size(comparison, code);
			removeTrailingComma(code);
			code.append("});\n");
			code.append(containerVariables.peek()).append(".addContained(").append(variable).append(");\n");
		}

		@Override
		public void visitLookupDescription(LookupDescription lookup,
											boolean parentVisible,
											boolean parentEnabled)
		throws MetaDataException {
			if (dataGridVariable != null) {
				dataGridColumnInputWidget = lookup;
				return;
			}

			if (formVariable == null) {
				throw new MetaDataException("LookupDescription found without a form");
			}

			SmartClientFieldDefinition def = preProcessFormItem(lookup, "bizLookupDescription");
			size(lookup, code);
			disabled(lookup.getDisabledConditionName(), code);
			invisible(lookup.getInvisibleConditionName(), code);
			editable(lookup.getEditable(), code);
			disableLookupComponents(lookup, code);
        	code.append("canCreate:").append(def.getLookup().getCanCreate());
        	code.append(",canUpdate:").append(def.getLookup().getCanUpdate());

			code.append(",_view:view,");
			appendFilterParameters(lookup.getParameters(), code);

			Query query = def.getLookup().getQuery();

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
												boolean parentEnabled)
		throws MetaDataException {
			// do nothing
		}

		@Override
		public void visitLookup(Lookup lookup,
									boolean parentVisible,
									boolean parentEnabled) 
		throws MetaDataException {
			if (dataGridVariable != null) {
				dataGridColumnInputWidget = lookup;
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
									boolean parentEnabled)
		throws MetaDataException {
			// do nothing
		}

		@Override
		public void visitPassword(Password password, 
									boolean parentVisible,
									boolean parentEnabled)
		throws MetaDataException {
			if (dataGridVariable != null) {
				dataGridColumnInputWidget = password;
				return;
			}

			if (formVariable == null) {
				throw new MetaDataException("Password found without a form");
			}

			preProcessFormItem(password, "password");
			size(password, code);
			disabled(password.getDisabledConditionName(), code);
			invisible(password.getInvisibleConditionName(), code);
		}

		@Override
		public void visitedPassword(Password password,
										boolean parentVisible,
										boolean parentEnabled)
		throws MetaDataException {
			// do nothing
		}

		@Override
		public void visitRadio(Radio radio,
								boolean parentVisible,
								boolean parentEnabled) 
		throws MetaDataException {
			if (dataGridVariable != null) {
				dataGridColumnInputWidget = radio;
				return;
			}

			if (formVariable == null) {
				throw new MetaDataException("Radio found without a form");
			}

			preProcessFormItem(radio, "radioGroup");
			size(radio, code);
			if (Boolean.FALSE.equals(radio.getVertical())) {
				code.append("vertical:false,");
			}
			disabled(radio.getDisabledConditionName(), code);
			invisible(radio.getInvisibleConditionName(), code);
		}

		@Override
		public void visitedRadio(Radio radio,
									boolean parentVisible,
									boolean parentEnabled)
		throws MetaDataException {
			// do nothing
		}

		@Override
		public void visitSlider(Slider slider, 
									boolean parentVisible,
									boolean parentEnabled)
		throws MetaDataException {
			if (dataGridVariable != null) {
				dataGridColumnInputWidget = slider;
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
			size(slider, code);
			disabled(slider.getDisabledConditionName(), code);
			invisible(slider.getInvisibleConditionName(), code);
		}

		@Override
		public void visitedSlider(Slider slider,
									boolean parentVisible,
									boolean parentEnabled)
		throws MetaDataException {
			// do nothing
		}

		@Override
		public void visitSpinner(Spinner spinner, 
									boolean parentVisible,
									boolean parentEnabled)
		throws MetaDataException {
			if (dataGridVariable != null) {
				dataGridColumnInputWidget = spinner;
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
			size(spinner, code);
			disabled(spinner.getDisabledConditionName(), code);
			invisible(spinner.getInvisibleConditionName(), code);
		}

		@Override
		public void visitedSpinner(Spinner spinner,
									boolean parentVisible,
									boolean parentEnabled)
		throws MetaDataException {
			// do nothing
		}

		@Override
		public void visitTextArea(TextArea text, 
									boolean parentVisible,
									boolean parentEnabled)
		throws MetaDataException {
			if (dataGridVariable != null) {
				dataGridColumnInputWidget = text;
				return;
			}

			if (formVariable == null) {
				throw new MetaDataException("TextArea found without a form");
			}

			preProcessFormItem(text, "textArea");
			size(text, code);
			disabled(text.getDisabledConditionName(), code);
			invisible(text.getInvisibleConditionName(), code);
			
			// Highlight text on focus
			code.append("selectOnFocus:true,");
		}

		@Override
		public void visitedTextArea(TextArea text,
										boolean parentVisible,
										boolean parentEnabled)
		throws MetaDataException {
			// do nothing
		}

		@Override
		public void visitTextField(TextField text,
									boolean parentVisible,
									boolean parentEnabled)
		throws MetaDataException {
			if (dataGridVariable != null) {
				dataGridColumnInputWidget = text;
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
				code.append("optionDataSource:BizUtil.PREVIOUS_VALUES_DATA_SOURCE,");
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
				
// do I need these ones also?
/*
pickListFields:[{name:'value'}],
*/
			}
			else {
				preProcessFormItem(text, null);
			}

			size(text, code);
			disabled(text.getDisabledConditionName(), code);
			invisible(text.getInvisibleConditionName(), code);
			
			// Highlight text on focus
			code.append("selectOnFocus:true,");
		}

		@Override
		public void visitedTextField(TextField text,
										boolean parentVisible,
										boolean parentEnabled)
		throws MetaDataException {
			// do nothing
		}

		@Override
		public void visitInject(Inject inject, boolean parentVisible, boolean parentEnabled)
		throws MetaDataException {
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
				if (ViewType.edit.equals(view.getType())) {
					code.append("view.addContained(edit);");
				}
				else if (ViewType.create.equals(view.getType())) {
					code.append("view.addContained(create);");
				}
			}
			if (! viewHasAtLeastOneForm) {
				String var = "v" + variableCounter++;
				code.append("var ").append(var).append("=DynamicForm.create({});");
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
		public void visitAction(ActionImpl action) {
			addAction(action.getResourceName(), 
						null, 
						action.getDisplayName(),
						action.getInActionPanel(),
						action.getClientValidation(),
						action.getRelativeIconFileName(),
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
						action.getToolTip(),
						action.getConfirmationText(),
						action.getParameters(),
						action.getDisabledConditionName(),
						action.getInvisibleConditionName());
		}

		@Override
		public void visitOnChangedEventHandler(Changeable changeable,
												boolean parentVisible,
												boolean parentEnabled)
		throws MetaDataException {
			if (visitingListMembership) {
				code.append("changed:function(){");
			}
			else {
				code.append("changed:function(form,item,value){");
			}
		}

		@Override
		public void visitedOnChangedEventHandler(Changeable changeable,
													boolean parentVisible,
													boolean parentEnabled)
		throws MetaDataException {
			code.append("},");
		}

		@Override
		public void visitOnFocusEventHandler(Focusable blurable,
												boolean parentVisible,
												boolean parentEnabled)
		throws MetaDataException {
			code.append("focus:function(form,item){");
		}

		@Override
		public void visitedOnFocusEventHandler(Focusable blurable,
												boolean parentVisible,
												boolean parentEnabled)
		throws MetaDataException {
			code.append("},");
		}

		@Override
		public void visitOnBlurEventHandler(Focusable blurable,
												boolean parentVisible,
												boolean parentEnabled)
		throws MetaDataException {
			code.append("blur:function(form,item){");
		}

		@Override
		public void visitedOnBlurEventHandler(Focusable blurable,
												boolean parentVisible,
												boolean parentEnabled)
		throws MetaDataException {
			code.append("},");
		}

		// Used to sort out server-side events into the bizEditedForServer() method.
		private boolean inOnEditedEventHandler = false;

		@Override
		public void visitOnEditedEventHandler(Lookup lookup,
												boolean parentVisible,
												boolean parentEnabled)
		throws MetaDataException {
			inOnEditedEventHandler = true;
			code.append("bizEdited:function(form,item,value){");
		}

		@Override
		public void visitedOnEditedEventHandler(Lookup lookup,
													boolean parentVisible,
													boolean parentEnabled)
		throws MetaDataException {
			code.append("},");
			inOnEditedEventHandler = false;
		}

		// Used to sort out server-side events into the bizEditedForServer() method.
		private boolean inOnAddedEventHandler = false;

		@Override
		public void visitOnAddedEventHandler(Lookup lookup,
												boolean parentVisible,
												boolean parentEnabled)
		throws MetaDataException {
			inOnAddedEventHandler = true;
			code.append("bizAdded:function(form,item,value){");
		}

		@Override
		public void visitedOnAddedEventHandler(Lookup lookup,
												boolean parentVisible,
												boolean parentEnabled)
		throws MetaDataException {
			code.append("},");
			inOnAddedEventHandler = false;
		}

		@Override
		public void visitOnPickedEventHandler(Lookup lookup,
												boolean parentVisible,
												boolean parentEnabled)
		throws MetaDataException {
			code.append("bizPicked:function(form,item,value){");
		}

		@Override
		public void visitedOnPickedEventHandler(Lookup lookup,
													boolean parentVisible,
													boolean parentEnabled)
		throws MetaDataException {
			code.append("},");
		}

		@Override
		public void visitOnClearedEventHandler(Lookup lookup,
												boolean parentVisible,
												boolean parentEnabled)
		throws MetaDataException {
			code.append("bizCleared:function(form,item,value){");
		}

		@Override
		public void visitedOnClearedEventHandler(Lookup lookup,
													boolean parentVisible,
													boolean parentEnabled)
		throws MetaDataException {
			code.append("},");
		}

		private void writeOutServerSideCallbackMethodIfNecessary() {
			if (inOnAddedEventHandler) {
				code.append("},bizAddedForServer:function(form,item,value){");
			}
			if (inOnEditedEventHandler) {
				code.append("},bizEditedForServer:function(form,item,value){");
			}
		}
		
		@Override
		public void visitRerenderEventAction(RerenderEventAction rerender,
												boolean parentVisible,
												boolean parentEnabled)
		throws MetaDataException {
			if (visitingListMembership) {
				code.append("this._view.rerender();");
			}
			else {
				writeOutServerSideCallbackMethodIfNecessary();
				code.append("form._view.rerender();");
			}
		}

		@Override
		public void visitServerSideActionEventAction(ServerSideActionEventAction server,
														boolean parentVisible,
														boolean parentEnabled)
		throws MetaDataException {
			writeOutServerSideCallbackMethodIfNecessary();
			Action action = view.getAction(server.getActionName());
			code.append("form._view.doAction('").append(server.getActionName()).append("',");
			code.append(! Boolean.FALSE.equals(action.getClientValidation())).append(");");
		}

		@Override
		public void visitSetDisabledEventAction(SetDisabledEventAction setDisabled,
													boolean parentVisible,
													boolean parentEnabled)
		throws MetaDataException {
			code.append("form._view.setDisabled('").append(setDisabled.getBinding().replace('.', '_'));
			code.append("','").append(setDisabled.getDisabledConditionName()).append("');");
		}

		@Override
		public void visitSetInvisibleEventAction(SetInvisibleEventAction setInvisible,
													boolean parentVisible,
													boolean parentEnabled)
		throws MetaDataException {
			code.append("form._view.setInvisible('").append(setInvisible.getBinding().replace('.', '_'));
			code.append("','").append(setInvisible.getInvisibleConditionName()).append("');");
		}

		@Override
		public void visitToggleDisabledEventAction(ToggleDisabledEventAction toggleDisabled,
													boolean parentVisible,
													boolean parentEnabled)
		throws MetaDataException {
			code.append("form._view.toggleDisabled('").append(toggleDisabled.getBinding().replace('.', '_'));
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
			result.append(view.getType()).append('_');
			result.append("'+").append(module.getName()).append('.').append(document.getName());
			if (ViewType.edit.equals(view.getType())) {
				result.append(SmartClientWebContext.EDIT_ID_COUNTER).append("++");
			}
			else if (ViewType.create.equals(view.getType())) {
				result.append(SmartClientWebContext.CREATE_ID_COUNTER).append("++");
			}
			
			return result.toString();
		}
		
		// TODO do we need to set size to "*"????
		private void size(AbsoluteWidth sizable, StringBuilder builder) {
			Integer width = sizable.getPixelWidth();
			boolean specifiedWidth = false;
			if (width != null) {
				builder.append("width:").append(width).append(',');
				specifiedWidth = true;
			}
			else {
				if (sizable instanceof RelativeSize) {
					width = ((RelativeSize) sizable).getPercentageWidth();
					if (width != null) {
						builder.append("width:'").append(width).append("%',");
						specifiedWidth = true;
					}
				}
			}
			if ((! specifiedWidth) && 
					(visitedItem != null) && 
					(! (sizable instanceof ContentSpecifiedWidth))) {
				builder.append("width:'*',");
			}
			
			if (sizable instanceof AbsoluteSize) {
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

		private static void bordered(Bordered bordered, StringBuilder builder) {
			if (Boolean.TRUE.equals(bordered.getBorder())) {
				String borderTitle = bordered.getBorderTitle();
				if (borderTitle != null) {
					builder.append("isGroup:true,margin:1,groupTitle:'").append(borderTitle).append("',");
				}
				else {
					builder.append("border:'1px solid #A7ABB4;',margin:1,");
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

		private void addAction(String actionName,
								ImplicitActionName implicitName,
								String displayName,
								Boolean inActionPanel,
								Boolean clientValidation,
								String relativeIconFileName,
								String tooltip,
								String confirmationText,
								List<Parameter> parameters,
								String disabledConditionName,
								String invisibleConditionName) {
			if (! Boolean.FALSE.equals(inActionPanel) && 
					(! ImplicitActionName.Add.equals(implicitName)) &&
					(! ImplicitActionName.Edit.equals(implicitName))) {
				String buttonCode = generateButton(actionName,
													implicitName,
													displayName,
													clientValidation,
													relativeIconFileName,
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
						code.append(ViewType.edit.equals(view.getType()) ? "Edit" : "Create");
					}
					code.append("Action(");
					code.append(buttonCode).append(");");
				}
			}
		}

		// return null if the button should NOT be added
		private String generateButton(String actionName,
										ImplicitActionName implicitName,
										String displayName,
										Boolean clientValidation,
										String relativeIconFileName,
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
				if (! user.canExecuteAction(document, actionName)) {
					return null; // cannot execute this action
				}
				result.append("BizButton.create({validate:");
				result.append(! Boolean.FALSE.equals(clientValidation));
				result.append(",actionName:'").append(actionName);
			}
			else {
				result.append("BizButton.create({actionName:'");
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
					if (! user.canReadDocument(document)) {
						return null;
					}
					if (revisedRelativeIconFileName == null) {
						revisedRelativeIconFileName = "actions/BizExport.png";
					}
					actionType = 'X';
					result.append(actionName);
					break;
				case BizImport:
					if ((! user.canCreateDocument(document)) || 
							(! user.canUpdateDocument(document))) {
						return null;
					}
					if (revisedRelativeIconFileName == null) {
						revisedRelativeIconFileName = "actions/BizImport.png";
					}
					actionType = 'I';
					result.append(actionName);
					break;
				case Upload:
					if (revisedRelativeIconFileName == null) {
						revisedRelativeIconFileName = "actions/Upload.png";
					}
					actionType = 'U';
					result.append(actionName);
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
				default:
					throw new IllegalArgumentException(implicitName + " not catered for");
				}
			}
			result.append("',type:'");
			result.append(actionType);
			result.append("',displayName:'");
			result.append((displayName == null) ? implicitName : SmartClientGenerateUtils.processString(displayName));
			result.append("',tabIndex:999,");
			if (revisedRelativeIconFileName != null) {
				result.append("icon:'").append("../resources?_doc=");
				result.append(module.getName()).append('.').append(document.getName());
				result.append("&_n=").append(revisedRelativeIconFileName).append("',");
			}
			if (button != null) {
				size(button, result);
			}
			disabled(disabledConditionName, result);
			invisible(invisibleConditionName, result);
			if (tooltip != null) {
				result.append("tooltip:'").append(SmartClientGenerateUtils.processString(tooltip)).append("',");
			}
			if (confirmationText != null) {
				result.append("confirm:'").append(SmartClientGenerateUtils.processString(confirmationText)).append("',");
			}
			appendParameters(parameters, result);
			result.append("_view:view})");
			
			return result.toString();
		}

		private static void appendParameters(List<Parameter> parameters, StringBuilder builder) {
			if ((parameters != null) && (! parameters.isEmpty())) {
				builder.append("params:{");
				for (Parameter parameter : parameters) {
					builder.append("'").append(parameter.getName().replace('.', '_')).append("':'");
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
					builder.append("{name:'").append(parameter.getName().replace('.', '_')).append("',operator:'");
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
																String typeOverride)
		throws MetaDataException {
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
				def.setTitle(title);
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

	@Override
	protected void doGet(HttpServletRequest request,
							HttpServletResponse response)
	throws ServletException, IOException {
		UtilImpl.LOGGER.info("SmartClient Generate - get....");
		processRequest(request, response);
	}

	@Override
	protected void doPost(HttpServletRequest request,
							HttpServletResponse response)
	throws ServletException, IOException {
		UtilImpl.LOGGER.info("SmartClient Generate - post....");
		processRequest(request, response);
	}

	// NB - Never throw ServletException as this will halt the SmartClient Relogin flow.
	private static void processRequest(HttpServletRequest request,
										HttpServletResponse response)
	throws IOException {
		String moduleName = request.getParameter(AbstractWebContext.MODULE_NAME);
		String documentName = request.getParameter(AbstractWebContext.DOCUMENT_NAME);

		response.setContentType(MimeType.javascript.toString());
		response.setCharacterEncoding(ServletConstants.UTF8);
		response.addHeader("Cache-control", "private,no-cache,no-store"); // never
		response.addDateHeader("Expires", 0); // never
		try (PrintWriter pw = response.getWriter()) {
			AbstractPersistence persistence = AbstractPersistence.get();
			try {
				persistence.begin();
				Principal userPrincipal = request.getUserPrincipal();
				User user = WebUtil.processUserPrincipalForRequest(request, 
																	(userPrincipal == null) ? null : userPrincipal.getName(),
																	true);
				if (user == null) {
					throw new SessionEndedException();
				}
				Customer customer = user.getCustomer();
	
				if (moduleName == null) {
					throw new ServletException("No module name in the request.");
				}
				if (documentName == null) {
					throw new ServletException("No document name in the request.");
				}
	
				Module module = customer.getModule(moduleName);
				Document document = module.getDocument(customer, documentName);
				View editView = document.getView(UX_UI, customer, ViewType.edit);
				View createView = document.getView(UX_UI, customer, ViewType.create);
	
				String editString = null;
				String createString = null;
	
				// create and edit view are the same - use edit view
				if (ViewType.edit.equals(createView.getType())) {
					SmartClientViewVisitor visitor = new SmartClientViewVisitor(user, customer, module, document, editView, true);
					visitor.visit();
					editString = visitor.getCode().toString();
				}
				else {
					SmartClientViewVisitor visitor = new SmartClientViewVisitor(user, customer, module, document, editView, false);
					visitor.visit();
					editString = visitor.getCode().toString();
	
					visitor = new SmartClientViewVisitor(user, customer, module, document, createView, false);
					visitor.visit();
					createString = visitor.getCode().toString();
				}
	
				pw.append(module.getName()).append('.').append(document.getName()).append(SmartClientWebContext.EDIT_ID_COUNTER).append("=0;");
				pw.append(module.getName()).append('.').append(document.getName()).append(SmartClientWebContext.CREATE_ID_COUNTER).append("=0;");
				pw.append(module.getName()).append(".create").append(document.getName()).append("=function(){");
				pw.append("var view=EditView.create({width:'100%',height:'100%',title:'");
				pw.append("',_mod:'").append(module.getName()).append("',_doc:'").append(document.getName());
				pw.append("',_ecnt:").append(module.getName()).append('.').append(document.getName()).append("_ecnt");
				pw.append(",_ccnt:").append(module.getName()).append('.').append(document.getName()).append("_ccnt});");

				pw.append(editString);
				if (createString != null) {
					pw.append(createString);
				}
	
				pw.append("return view;};");
			}
			catch (Throwable t) {
				t.printStackTrace();
				persistence.rollback();
	
				pw.append("isc.warn('");
				if (t instanceof MessageException) {
					SmartClientEditServlet.appendErrorText("Could not generate view.",
															((MessageException) t).getMessages(),
															pw);
					pw.append("');");
				}
				else {
					pw.append("isc.warn('Could not generate views.  Please contact your system administrator.');");
				}
			}
			finally {
				if (persistence != null) {
					persistence.commit(true);
				}
			}
		}
	}
}