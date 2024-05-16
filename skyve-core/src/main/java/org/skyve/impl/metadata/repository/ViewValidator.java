package org.skyve.impl.metadata.repository;

import java.awt.Color;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;

import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;
import org.skyve.domain.types.formatters.Formatter;
import org.skyve.domain.types.formatters.Formatters;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.repository.behaviour.ActionMetaData;
import org.skyve.impl.metadata.view.AbsoluteSize;
import org.skyve.impl.metadata.view.AbsoluteWidth;
import org.skyve.impl.metadata.view.ActionImpl;
import org.skyve.impl.metadata.view.Inject;
import org.skyve.impl.metadata.view.RelativeSize;
import org.skyve.impl.metadata.view.RelativeWidth;
import org.skyve.impl.metadata.view.ResponsiveWidth;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.metadata.view.ViewVisitor;
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
import org.skyve.impl.metadata.view.event.EventAction;
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
import org.skyve.impl.metadata.view.model.chart.ChartBuilderMetaData;
import org.skyve.impl.metadata.view.reference.ActionReference;
import org.skyve.impl.metadata.view.reference.ContentReference;
import org.skyve.impl.metadata.view.reference.DefaultListViewReference;
import org.skyve.impl.metadata.view.reference.EditViewReference;
import org.skyve.impl.metadata.view.reference.ExternalReference;
import org.skyve.impl.metadata.view.reference.ImplicitActionReference;
import org.skyve.impl.metadata.view.reference.QueryListViewReference;
import org.skyve.impl.metadata.view.reference.ReferenceProcessor;
import org.skyve.impl.metadata.view.reference.ReportReference;
import org.skyve.impl.metadata.view.reference.ResourceReference;
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
import org.skyve.impl.metadata.view.widget.bound.ParameterImpl;
import org.skyve.impl.metadata.view.widget.bound.ProgressBar;
import org.skyve.impl.metadata.view.widget.bound.ZoomIn;
import org.skyve.impl.metadata.view.widget.bound.input.CheckBox;
import org.skyve.impl.metadata.view.widget.bound.input.CheckMembership;
import org.skyve.impl.metadata.view.widget.bound.input.ColourPicker;
import org.skyve.impl.metadata.view.widget.bound.input.Combo;
import org.skyve.impl.metadata.view.widget.bound.input.Comparison;
import org.skyve.impl.metadata.view.widget.bound.input.ContentImage;
import org.skyve.impl.metadata.view.widget.bound.input.ContentLink;
import org.skyve.impl.metadata.view.widget.bound.input.ContentSignature;
import org.skyve.impl.metadata.view.widget.bound.input.Geometry;
import org.skyve.impl.metadata.view.widget.bound.input.GeometryMap;
import org.skyve.impl.metadata.view.widget.bound.input.HTML;
import org.skyve.impl.metadata.view.widget.bound.input.ListMembership;
import org.skyve.impl.metadata.view.widget.bound.input.LookupDescription;
import org.skyve.impl.metadata.view.widget.bound.input.LookupDescriptionColumn;
import org.skyve.impl.metadata.view.widget.bound.input.Password;
import org.skyve.impl.metadata.view.widget.bound.input.Radio;
import org.skyve.impl.metadata.view.widget.bound.input.RichText;
import org.skyve.impl.metadata.view.widget.bound.input.Slider;
import org.skyve.impl.metadata.view.widget.bound.input.Spinner;
import org.skyve.impl.metadata.view.widget.bound.input.TextArea;
import org.skyve.impl.metadata.view.widget.bound.input.TextField;
import org.skyve.impl.metadata.view.widget.bound.tabular.AbstractDataWidget;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridBoundColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridContainerColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataRepeater;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListRepeater;
import org.skyve.impl.metadata.view.widget.bound.tabular.TreeGrid;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.metadata.FilterOperator;
import org.skyve.metadata.FormatterName;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.Extends;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Reference;
import org.skyve.metadata.model.document.Relation;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryColumn;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.module.query.MetaDataQueryProjectedColumn;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.metadata.view.Action;
import org.skyve.metadata.view.Action.ActionShow;
import org.skyve.metadata.view.View.ViewParameter;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.metadata.view.widget.FilterParameter;
import org.skyve.metadata.view.widget.bound.Bound;
import org.skyve.metadata.view.widget.bound.Parameter;
import org.skyve.persistence.DocumentQuery.AggregateFunction;
import org.skyve.report.ReportFormat;
import org.skyve.util.Binder;
import org.skyve.util.Binder.TargetMetaData;

// TODO check suggestion attributes on text fields etc
class ViewValidator extends ViewVisitor {
	private ProvidedRepository repository;
	
	private String viewIdentifier;
	
	// These 2 variables are used when validating the contents of a data grid / data repeater
	private String dataWidgetIdentifier;
	private String dataWidgetBinding;
	
	ViewValidator(ViewImpl view, 
					ProvidedRepository repository, 
					CustomerImpl customer, 
					DocumentImpl document, 
					String uxui) {
		super(customer, (ModuleImpl) repository.getModule(customer, document.getOwningModuleName()), document, view, uxui);
		this.repository = repository;
		viewIdentifier = view.getName() + " view for UX/UI " + uxui + " for document " + module.getName() + '.' + document.getName();
	}

	private Class<?> validateBinding(String bindingPrefix,
										String binding, 
										boolean bindingRequired,
										boolean compoundBindingInvalid, 
										boolean domainValuesRequired,
										boolean scalarBindingOnly,
										String widgetIdentifier,
										AttributeType... assertTypes) {
		return validateBinding(module,
								document,
								bindingPrefix,
								binding,
								bindingRequired,
								compoundBindingInvalid,
								domainValuesRequired,
								scalarBindingOnly,
								widgetIdentifier,
								assertTypes);
	}

	private Class<?> validateBinding(Module contextModule,
										Document contextDocument,
										String bindingPrefix,
										String binding, 
										boolean bindingRequired,
										boolean compoundBindingInvalid, 
										boolean domainValuesRequired,
										boolean scalarBindingOnly,
										String widgetIdentifier,
										AttributeType... assertTypes) {
		if (bindingRequired && (binding == null)) {
			throw new MetaDataException(widgetIdentifier + " in " + viewIdentifier + " - binding is required.");
		}
		if (binding == null) {
			return null;
		}

		if (compoundBindingInvalid) {
			if (binding.indexOf('.') >= 0) {
				throw new MetaDataException(widgetIdentifier + " in " + viewIdentifier + " - Compound binding is not allowed here");
			}
		}
		String bindingToTest = binding;
		if (bindingPrefix != null) {
			bindingToTest = new StringBuilder(64).append(bindingPrefix).append('.').append(binding).toString();
		}

		TargetMetaData target = null;
		try {
			target = BindUtil.validateBinding(customer, contextModule, contextDocument, bindingToTest);
		}
		catch (MetaDataException e) {
			throw new MetaDataException(widgetIdentifier + " in " + viewIdentifier + " has an invalid binding of " + binding, e);
		}
		Attribute attribute = target.getAttribute();
		AttributeType attributeType = (attribute == null) ? null : attribute.getAttributeType();
		Class<?> result = target.getType();

		if (domainValuesRequired) {
			if (attribute == null) {
				throw new MetaDataException(widgetIdentifier + " in " + viewIdentifier + 
												" - Binding points to an implicit attribute or a condition that cannot have domain values defined.");
			}
			if (attribute.getDomainType() == null) {
				throw new MetaDataException(widgetIdentifier + " in " + viewIdentifier + 
												" - Binding points to an attribute that does not have domain values defined.");
			}
		}

		if ((assertTypes != null) && (assertTypes.length > 0)) {
			// If we have a parent binding it had better be used where an association binding can be used (and not require domain values)
			if (ChildBean.PARENT_NAME.equals(binding) || binding.endsWith(ChildBean.CHILD_PARENT_NAME_SUFFIX)) {
				boolean typeMatch = false;
				for (AttributeType assertType : assertTypes) {
					if (assertType.equals(AttributeType.association)) {
						typeMatch = true;
						break;
					}
				}
				if (! typeMatch) {
					throw new MetaDataException(widgetIdentifier + " in " + viewIdentifier + 
							" - Parent binding used where an association is not valid.");
				}
			}
			else {
				if (attribute == null) {
					throw new MetaDataException(widgetIdentifier + " in " + viewIdentifier + 
													" - Binding points to an implicit attribute or a condition.");
				}

				boolean typeMatch = false;
				for (AttributeType assertType : assertTypes) {
					if (assertType.equals(attributeType)) {
						typeMatch = true;
						break;
					}
				}
				if (! typeMatch) {
					StringBuilder msg = new StringBuilder(128);
					msg.append(widgetIdentifier).append(" in ").append(viewIdentifier);
					msg.append(" - Binding points to an attribute of type ").append(attributeType).append(", not one of ");
					for (AttributeType assertType : assertTypes) {
						msg.append(assertType).append(", ");
					}
					msg.setLength(msg.length() - 2); // remove last comma
					msg.append('.');
					throw new MetaDataException(msg.toString());
				}
			}
		}
		
		// Can only check this if the attribute is defined.
		// Bindings to implicit attributes are always scalar.
		// NB check assert type in outer if coz we don't need to do the test if we are asserting a type
		if (scalarBindingOnly && ((assertTypes == null) || (assertTypes.length == 0)) && (attribute != null)) {
			if (AttributeType.association.equals(attributeType) || 
					AttributeType.collection.equals(attributeType) || 
					AttributeType.inverseMany.equals(attributeType) ||
					AttributeType.inverseOne.equals(attributeType)) {
				throw new MetaDataException(widgetIdentifier + " in " + viewIdentifier +
												" - Binding points to an attribute that is not scalar (pointing to an association or collection or inverse)");
			}
		}
		
		return result;
	}
	
	private void validateConditionName(String conditionName, String widgetIdentifier) {
		// ignore true and false when checking the condition exists
		if ((conditionName != null) && (! "true".equals(conditionName)) && (! "false".equals(conditionName))) {
			// reverse the sense of the condition if it starts with "not"
			String testConditionName = conditionName;
			if (testConditionName.startsWith("not")) {
				if (! Character.isUpperCase(testConditionName.charAt(3))) {
					throw new MetaDataException(widgetIdentifier + " in " + viewIdentifier + 
													"references condition " + conditionName + " which is not correctly camel cased (eg notTrue)");
				}
				testConditionName = Character.toLowerCase(testConditionName.charAt(3)) + testConditionName.substring(4);
			}

			// ignore implicit conditions when checking the condition exists
			if ((! Bean.PERSISTED_KEY.equals(testConditionName)) && 
					(! Bean.CREATED_KEY.equals(testConditionName)) &&
					(! Bean.CHANGED_KEY.equals(testConditionName))) {
				validateCondition(module, document, testConditionName, widgetIdentifier);
			}
		}
	}
	
	private void validateCondition(ModuleImpl currentModule, 
									DocumentImpl currentDocument,
									String testConditionName,
									String widgetIdentifier) {
		if (! currentDocument.getConditionNames().contains(testConditionName)) {
			Extends extension = currentDocument.getExtends();
			if (extension == null) {
				throw new MetaDataException(widgetIdentifier + " in " + viewIdentifier + 
												" references condition " + testConditionName + " which does not exist");
			}
			
			DocumentImpl baseDocument = (DocumentImpl) currentModule.getDocument(customer, extension.getDocumentName());
			ModuleImpl baseModule = (ModuleImpl) repository.getModule(customer, baseDocument.getOwningModuleName());
			validateCondition(baseModule, baseDocument, testConditionName, widgetIdentifier);
		}
	}

	private void validateParameterBindings(List<Parameter> parameters, String parentWidgetIdentifier) {
		if (parameters != null) {
			for (Parameter parameter : parameters) {
				validateBinding(null,
									parameter.getValueBinding(),
									false,
									false,
									false,
									false,
									"Parameter " + parameter.getName() + " in " + parentWidgetIdentifier);
			}
		}
	}
	
	private void validateFilterParameterBindings(List<FilterParameter> parameters,
													String parentWidgetIdentifier,
													Document drivingDocument) {
		if (parameters != null) {
			Module drivingModule = (drivingDocument == null) ? null : repository.getModule(customer, drivingDocument.getOwningModuleName());

			for (FilterParameter parameter : parameters) {
				String filterBinding = parameter.getFilterBinding();
				String widgetIdentifier = new StringBuilder(128).append("Filter Parameter ").append(filterBinding).append(" in ").append(parentWidgetIdentifier).toString();
				validateBinding(null,
									parameter.getValueBinding(),
									false,
									false,
									false,
									false,
									widgetIdentifier);
				if (drivingDocument != null) {
					validateBinding(drivingModule,
										drivingDocument,
										null,
										filterBinding,
										false,
										false,
										false,
										false,
										widgetIdentifier);
				}
			}
		}
	}

	private void validateMessageExpressions(String message, 
												String widgetIdentifier,
												String description) {
		if (message != null) {
			Document testDocument = document;
			if (dataWidgetBinding != null) {
				TargetMetaData target = BindUtil.getMetaDataForBinding(customer, module, document, dataWidgetBinding);
				Attribute targetAttribute = target.getAttribute();
				// Collection and Inverse are appropriate here...
				if (targetAttribute instanceof Relation) {
					Relation relation = (Relation) targetAttribute;
					testDocument = module.getDocument(customer, relation.getDocumentName());
				}
			}
			String error = BindUtil.validateMessageExpressions(message, customer, testDocument);
			if (error != null) {
				throw new MetaDataException(widgetIdentifier + " in " + viewIdentifier + 
												" has " + description + " containing malformed binding expressions: " + error);
			}
		}
	}
	
	private void validateNoColonInParameter(List<Parameter> parameters, String widgetIdentifier) {
		if (parameters != null) {
			for (Parameter parameter : parameters) {
				if (parameter.getName().indexOf(':') >= 0) {
					throw new MetaDataException(widgetIdentifier + " in " + viewIdentifier + " has a parameter named " + parameter.getName() + 
													" which contains a colon (:). Use a parameter to bind a value to a named parameter in a query");
				}
			}
		}
	}

	private void validateNoColonInFilterParameter(List<FilterParameter> parameters, String widgetIdentifier) {
		if (parameters != null) {
			for (FilterParameter parameter : parameters) {
				if (parameter.getFilterBinding().indexOf(':') >= 0) {
					throw new MetaDataException(widgetIdentifier + " in " + viewIdentifier + " has a parameter with filterBinding " + parameter.getFilterBinding() + 
													" which contains a colon (:). Use a parameter to bind a value to a named parameter in a query");
				}
			}
		}
	}

	private Document validateQueryOrModel(String queryName, String modelName, String widgetIdentifier) {
		Document result = null;
		if (queryName != null) {
			if (modelName != null) {
				throw new MetaDataException(widgetIdentifier + " in " + viewIdentifier + " has a query and a model name.");
			}
			result = validateQueryName(queryName, widgetIdentifier);
		}
		else if (modelName != null) {
			result = validateListModelName(modelName, widgetIdentifier);
		}
		else {
			throw new MetaDataException(widgetIdentifier + " in " + viewIdentifier + " requires a query name or a model name.");
		}
		return result;
	}

	private Document validateQueryName(String queryName, String widgetIdentifier) {
		Document result = null;
		if (queryName != null) {
			MetaDataQueryDefinition query = module.getMetaDataQuery(queryName);
			if (query == null) {
				throw new MetaDataException(widgetIdentifier + " in " + viewIdentifier + " does not reference a valid query of " + queryName);
			}
			Module m = query.getDocumentModule(customer);
			result = m.getDocument(customer, query.getDocumentName());
		}
		return result;
 	}
	
	private Document validateListModelName(String modelName, String widgetIdentifier) {
		Document result = null;
		if (modelName != null) {
			try {
				ListModel<Bean> model = repository.getListModel(customer, document, modelName, false);
				// Check driving document can be obtained to ensure bindings and accesses can be calculated
				result = model.getDrivingDocument();
			}
			catch (Exception e) {
				throw new MetaDataException(widgetIdentifier + " in " + viewIdentifier + " does not reference a valid list model of " + modelName, e);
			}
		}
		return result;
	}

	private void validateMapModelName(String modelName, String widgetIdentifier) {
		if (modelName != null) {
			try {
				repository.getMapModel(customer, document, modelName, false);
			}
			catch (Exception e) {
				throw new MetaDataException(widgetIdentifier + " in " + viewIdentifier + " does not reference a valid map model of " + modelName, e);
			}
		}
	}

	private void validateChartModelName(String modelName, String widgetIdentifier) {
		if (modelName != null) {
			try {
				repository.getChartModel(customer, document, modelName, false);
			}
			catch (Exception e) {
				throw new MetaDataException(widgetIdentifier + " in " + viewIdentifier + " does not reference a valid chart model of " + modelName, e);
			}
		}
	}

	private void validateComparisonModelName(String modelName, String widgetIdentifier) {
		if (modelName != null) {
			try {
				repository.getComparisonModel(customer, document, modelName, false);
			}
			catch (Exception e) {
				throw new MetaDataException(widgetIdentifier + " in " + viewIdentifier + " does not reference a valid comparison model of " + modelName, e);
			}
		}
	}
	
	private void validateActionName(String actionName, String widgetIdentifier) {
		if ((actionName != null) && (view.getAction(actionName) == null)) {
			throw new MetaDataException(widgetIdentifier + " in " + viewIdentifier + " references a non-existent action " + actionName);
		}
	}
	
	private void validateSize(AbsoluteWidth sizable, String widgetIdentifier) {
		validatePositive(sizable.getPixelWidth(), widgetIdentifier, "pixelWidth");
		if (sizable instanceof AbsoluteSize) {
			validatePositive(((AbsoluteSize) sizable).getPixelHeight(), widgetIdentifier, "pixelHeight");
			if (sizable instanceof RelativeWidth) {
				RelativeWidth width = (RelativeWidth) sizable;
				validatePercentage(width.getPercentageWidth(), widgetIdentifier, "percentageWidth");
				validateResponsive(width.getResponsiveWidth(), widgetIdentifier, "responsiveWidth");
				if (sizable instanceof ResponsiveWidth) {
					ResponsiveWidth responsive = (ResponsiveWidth) sizable;
					validateResponsive(responsive.getSm(), widgetIdentifier, "sm");
					validateResponsive(responsive.getMd(), widgetIdentifier, "md");
					validateResponsive(responsive.getLg(), widgetIdentifier, "lg");
					validateResponsive(responsive.getXl(), widgetIdentifier, "xl");
					if (sizable instanceof RelativeSize) {
						RelativeSize relative = (RelativeSize) sizable;
						validatePositive(relative.getMinPixelWidth(), widgetIdentifier, "minPixelWidth");
						validatePositive(relative.getMaxPixelWidth(), widgetIdentifier, "maxPixelWidth");
						validatePositive(relative.getMinPixelHeight(), widgetIdentifier, "minPixelHeight");
						validatePositive(relative.getMaxPixelHeight(), widgetIdentifier, "maxPixelHeight");
						validatePercentage(relative.getPercentageHeight(), widgetIdentifier, "percentageHeight");
						if (sizable instanceof Box) {
							Box box = (Box) relative;
							validatePositive(box.getPixelPadding(), widgetIdentifier, "pixelPadding");
							validatePositive(box.getPixelMemberPadding(), widgetIdentifier, "pixelPadding");
						}
					}
				}
			}
		}
	}
	
	private void validatePositive(Integer size, String widgetIdentifier, String sizeAttributeName) {
		if ((size != null) && (size.intValue() <= 0)) {
			throw new MetaDataException(widgetIdentifier + " in " + viewIdentifier + " has a value for " + sizeAttributeName + " of " + size + " that is not positive");
		}
	}

	private void validatePercentage(Integer size, String widgetIdentifier, String sizeAttributeName) {
		if (size != null) {
			int value = size.intValue();
			if ((value <= 0) || (value > 100)) {
				throw new MetaDataException(widgetIdentifier + " in " + viewIdentifier + " has a value for " + sizeAttributeName + " of " + size + " that is not between 1 and 100");
			}
		}
	}

	private void validateResponsive(Integer size, String widgetIdentifier, String sizeAttributeName) {
		if (size != null) {
			int value = size.intValue();
			if ((value <= 0) || (value > 12)) {
				throw new MetaDataException(widgetIdentifier + " in " + viewIdentifier + " has a value for " + sizeAttributeName + " of " + size + " that is not between 1 and 12");
			}
		}
	}

	private void validateCollapsible(Collapsible collapsible, String borderTitle, String widgetIdentifier) {

		if (collapsible != null && borderTitle == null) {
			throw new MetaDataException(widgetIdentifier + " in " + viewIdentifier
					+ " must have a border title if the collapsible attribute is present");
		}

	}

	@Override
	public void visitButton(Button button, boolean parentVisible, boolean parentEnabled) {
		String actionName = button.getActionName();
		String buttonIdentifier = "A button " + button.getActionName();
		validateActionName(actionName, buttonIdentifier);
		validateSize(button, buttonIdentifier);
		
		ActionShow show = button.getShow();
		if (ActionShow.icon == show) {
			Action action = view.getAction(actionName); // validated to exist above
			if ((action.getIconStyleClass() == null) && (action.getRelativeIconFileName() == null)) {
				throw new MetaDataException(buttonIdentifier + " in " + viewIdentifier + " is set to [show] an icon but " + actionName + " has no [iconStyleClass] or [relativeIconFileName] defined.");
			}
		}
	}

	@Override
	public void visitZoomIn(ZoomIn zoomIn, boolean parentVisible, boolean parentEnabled) {
		String binding = zoomIn.getBinding();
		String zoomInIdentifier = "ZoomIn " + binding;
		validateBinding(null,
							binding,
							true,
							false,
							false,
							false,
							zoomInIdentifier,
							AttributeType.association,
							AttributeType.inverseOne);
		validateConditionName(zoomIn.getDisabledConditionName(), zoomInIdentifier);
		validateConditionName(zoomIn.getInvisibleConditionName(), zoomInIdentifier);
		validateSize(zoomIn, zoomInIdentifier);
		
		if ((ActionShow.icon == zoomIn.getShow()) && 
				(zoomIn.getIconStyleClass() == null) &&
				(zoomIn.getRelativeIconFileName() == null)) {
			throw new MetaDataException(zoomInIdentifier + " in " + viewIdentifier + " is set to [show] an icon but has no [iconStyleClass] or [relativeIconFileName] defined.");
		}
	}

	@Override
	public void visitDynamicImage(DynamicImage image, boolean parentVisible, boolean parentEnabled) {
		String imageIdentifier = "Dynamic Image " + image.getName();
		validateConditionName(image.getInvisibleConditionName(), imageIdentifier);
		validateParameterBindings(image.getParameters(), imageIdentifier);
		validateSize(image, imageIdentifier);
	}

	@Override
	public void visitCheckBox(CheckBox checkBox, boolean parentVisible, boolean parentEnabled) {
		String binding = checkBox.getBinding();
		String checkBoxIdentifier = "CheckBox " + binding;
		if (dataWidgetBinding != null) {
			checkBoxIdentifier += " in " + dataWidgetIdentifier;
		}
		validateBinding(dataWidgetBinding, 
							binding, 
							true, 
							false, 
							false, 
							true,
							checkBoxIdentifier,
							AttributeType.bool);
		validateConditionName(checkBox.getDisabledConditionName(), checkBoxIdentifier);
		validateConditionName(checkBox.getInvisibleConditionName(), checkBoxIdentifier);
		validateSize(checkBox, checkBoxIdentifier);
	}

	@Override
	public void visitedCheckBox(CheckBox checkBox,
									boolean parentVisible,
									boolean parentEnabled) {
		// do nothing
	}

	@Override
	public void visitCheckMembership(CheckMembership membership, boolean parentVisible, boolean parentEnabled) {
		String membershipIdentifier = "CheckBox " + membership.getBinding();
		validateBinding(null,
							membership.getBinding(),
							true,
							false,
							true,
							false,
							membershipIdentifier,
							AttributeType.collection);
		validateConditionName(membership.getDisabledConditionName(), membershipIdentifier);
		validateConditionName(membership.getInvisibleConditionName(), membershipIdentifier);
	}

	@Override
	public void visitedCheckMembership(CheckMembership membership,
										boolean parentVisible,
										boolean parentEnabled) {
		// do nothing
	}

	@Override
	public void visitColourPicker(ColourPicker colour,
									boolean parentVisible,
									boolean parentEnabled) {
		String binding = colour.getBinding();
		String colourIdentifier = "Colour " + binding;
		if (dataWidgetBinding != null) {
			colourIdentifier += " in " + dataWidgetIdentifier;
		}
		validateBinding(dataWidgetBinding,
							binding,
							true,
							false,
							false,
							true,
							colourIdentifier,
							AttributeType.colour);
		validateConditionName(colour.getDisabledConditionName(), colourIdentifier);
		validateConditionName(colour.getInvisibleConditionName(), colourIdentifier);
		validateSize(colour, colourIdentifier);
	}

	@Override
	public void visitedColourPicker(ColourPicker colour,
										boolean parentVisible,
										boolean parentEnabled) {
		// do nothing
	}

	@Override
	public void visitCombo(Combo combo, boolean parentVisible, boolean parentEnabled) {
		String binding = combo.getBinding();
		String comboIdentifier = "Combo " + binding;
		if (dataWidgetBinding != null) {
			comboIdentifier += " in " + dataWidgetIdentifier;
		}
		validateBinding(dataWidgetBinding,
							binding,
							true,
							false,
							true,
							false,
							comboIdentifier,
							AttributeType.enumeration,
							AttributeType.text,
							AttributeType.association,
							AttributeType.inverseOne,
							AttributeType.bool,
							AttributeType.integer,
							AttributeType.longInteger);
		validateConditionName(combo.getDisabledConditionName(), comboIdentifier);
		validateConditionName(combo.getInvisibleConditionName(), comboIdentifier);
		validateSize(combo, comboIdentifier);
	}

	@Override
	public void visitedCombo(Combo combo, boolean parentVisible, boolean parentEnabled) {
		// do nothing
	}

	@Override
	public void visitContentImage(ContentImage image, boolean parentVisible, boolean parentEnabled) {
		String binding = image.getBinding();
		String imageIdentifier = "ContentImage " + binding;
		if (dataWidgetBinding != null) {
			imageIdentifier += " in " + dataWidgetIdentifier;
		}
		validateBinding(dataWidgetBinding, 
							binding,
							true,
							false,
							false,
							true,
							imageIdentifier,
							AttributeType.content,
							AttributeType.image);
		validateConditionName(image.getDisabledConditionName(), imageIdentifier);
		validateConditionName(image.getInvisibleConditionName(), imageIdentifier);
		validateSize(image, imageIdentifier);
	}

	@Override
	public void visitContentLink(ContentLink link, boolean parentVisible, boolean parentEnabled) {
		String binding = link.getBinding();
		String linkIdentifier = "ContentLink " + link.getBinding();
		if (dataWidgetBinding != null) {
			linkIdentifier += " in " + dataWidgetIdentifier;
		}
		validateBinding(dataWidgetBinding,
							binding,
							false,
							false,
							false,
							true,
							linkIdentifier,
							AttributeType.content,
							AttributeType.image);
		validateConditionName(link.getDisabledConditionName(), linkIdentifier);
		validateConditionName(link.getInvisibleConditionName(), linkIdentifier);
		validateParameterBindings(link.getParameters(), linkIdentifier);
		validateSize(link, linkIdentifier);
	}

	@Override
	public void visitContentSignature(ContentSignature signature, boolean parentVisible, boolean parentEnabled) {
		String binding = signature.getBinding();
		String signatureIdentifier = "ContentSignature " + binding;
		validateBinding(dataWidgetBinding, 
							binding,
							true,
							false,
							false,
							true,
							signatureIdentifier,
							AttributeType.content,
							AttributeType.image);
		validateConditionName(signature.getDisabledConditionName(), signatureIdentifier);
		validateConditionName(signature.getInvisibleConditionName(), signatureIdentifier);
		validateSize(signature, signatureIdentifier);
		
		String colour = signature.getRgbHexBackgroundColour();
		if (colour != null) {
			try {
				Color.decode(colour);
			}
			catch (@SuppressWarnings("unused") NumberFormatException e) {
				throw new MetaDataException(signatureIdentifier + " in " + viewIdentifier + " has an invalid value for rgbHexBackgroundColour of " + colour + " (Should be formatted as #RRGGBB)");
			}
		}
		colour = signature.getRgbHexForegroundColour();
		if (colour != null) {
			try {
				Color.decode(colour);
			}
			catch (@SuppressWarnings("unused") NumberFormatException e) {
				throw new MetaDataException(signatureIdentifier + " in " + viewIdentifier + " has an invalid value for rgbHexForegroundColour of " + colour + " (Should be formatted as #RRGGBB)");
			}
		}
	}

	@Override
	public void visitDataGrid(DataGrid grid, boolean parentVisible, boolean parentEnabled) {
		visitDataWidget(grid, "DataGrid");
		validateBinding(null, grid.getSelectedIdBinding(), false, false, false, true, dataWidgetIdentifier, AttributeType.id);
		validateConditionName(grid.getDisabledConditionName(), dataWidgetIdentifier);
		validateConditionName(grid.getDisableAddConditionName(), dataWidgetIdentifier);
		validateConditionName(grid.getDisableEditConditionName(), dataWidgetIdentifier);
		validateConditionName(grid.getDisableRemoveConditionName(), dataWidgetIdentifier);
		validateConditionName(grid.getDisableZoomConditionName(), dataWidgetIdentifier);
	}

	@Override
	public void visitDataRepeater(DataRepeater repeater, boolean parentVisible, boolean parentEnabled) {
		visitDataWidget(repeater, "DataRepeater");
	}

	private void visitDataWidget(AbstractDataWidget widget, String widgetDescription) {
		String title = widget.getTitle();
		dataWidgetBinding = widget.getBinding();
		StringBuilder sb = new StringBuilder(64);
		sb.append(widgetDescription);
		String widgetId = widget.getWidgetId();
		if (widgetId != null) {
			sb.append(" with id ").append(widgetId);
		}
		if (title != null) {
			sb.append((sb.length() > 12) ? " and " : " with ").append("title ").append(title);
		}
		sb.append((sb.length() > 12) ? " and " : " with ").append("binding ").append(dataWidgetBinding);
		dataWidgetIdentifier = sb.toString();
		validateBinding(null,
							dataWidgetBinding,
							true,
							false,
							false,
							false,
							dataWidgetIdentifier,
							AttributeType.collection,
							AttributeType.inverseMany);
		validateConditionName(widget.getInvisibleConditionName(), dataWidgetIdentifier);
		validateSize(widget, dataWidgetIdentifier);
	}

	@Override
	public void visitDataGridBoundColumn(DataGridBoundColumn column,
											boolean parentVisible,
											boolean parentEnabled) {
		String columnIdentifier = "Column " + column.getTitle() + " of " + dataWidgetIdentifier;
		Class<?> columnType = validateBinding(dataWidgetBinding,
												column.getBinding(),
												false,
												false,
												false,
												false,
												columnIdentifier);
		FormatterName formatterName = column.getFormatterName();
		String customFormatterName = column.getCustomFormatterName();
		if ((formatterName != null) && (customFormatterName != null)) {
			throw new MetaDataException(columnIdentifier + " in " + viewIdentifier + " cannot have a both a formatter and a customFormatter defined.");
		}
		else if (formatterName != null) {
			if (columnType != null) {
				if (! formatterName.getFormatter().getValueType().isAssignableFrom(columnType)) {
					throw new MetaDataException(columnIdentifier + " in " + viewIdentifier + 
												" has formatter " + formatterName.name() + 
												" for type " + formatterName.getFormatter().getValueType() + 
												" but the column binding of type " + columnType + " is incompatible");
					
				}
			}
		}
		else if (customFormatterName != null) {
			Formatter<?> customFormatter = Formatters.get(customFormatterName);
			if (customFormatter == null) {
				throw new MetaDataException(columnIdentifier + " in " + viewIdentifier + " has a customFormatter of " + customFormatterName + " that is not defined");
			}
			if (! customFormatter.getValueType().isAssignableFrom(columnType)) {
				throw new MetaDataException(columnIdentifier + " in " + viewIdentifier + 
											" has customFormatter " + customFormatterName + 
											" for type " + customFormatter.getValueType() + 
											" but the column binding of type " + columnType + " is incompatible");
				
			}
		}
	}

	@Override
	public void visitedDataGridBoundColumn(DataGridBoundColumn column,
											boolean parentVisible, 
											boolean parentEnabled) {
		// nothing to do
	}

	@Override
	public void visitDataGridContainerColumn(DataGridContainerColumn column,
												boolean parentVisible,
												boolean parentEnabled) {
		// nothing to do
	}

	@Override
	public void visitedDataGridContainerColumn(DataGridContainerColumn column,
												boolean parentVisible, 
												boolean parentEnabled) {
		// nothing to do
	}

	@Override
	public void visitDialogButton(DialogButton button, boolean parentVisible, boolean parentEnabled) {
		String buttonIdentifier = "A Dialog Button" + button.getDialogName();
		validateConditionName(button.getDisabledConditionName(), buttonIdentifier);
		validateConditionName(button.getInvisibleConditionName(), buttonIdentifier);
		validateParameterBindings(button.getParameters(), buttonIdentifier);
	}

	@Override
	public void visitForm(Form form, boolean parentVisible, boolean parentEnabled) {
		String formIdentifier = form.getWidgetId();
		if (formIdentifier == null) {
			formIdentifier = "A Form";
		}
		else {
			formIdentifier = "Form " + formIdentifier;
		}
		validateConditionName(form.getDisabledConditionName(), formIdentifier);
		validateConditionName(form.getInvisibleConditionName(), formIdentifier);
		validateSize(form, formIdentifier);
		validateMessageExpressions(form.getLocalisedBorderTitle(), formIdentifier, "borderTitle");
		validateCollapsible(form.getCollapsible(), form.getBorderTitle(), formIdentifier);
	}

	@Override
	public void visitFormColumn(FormColumn column, boolean parentVisible, boolean parentEnabled) {
		String columnIdentifer = "A form column";
		validatePositive(column.getPixelWidth(), columnIdentifer, "pixelWidth");
		validatePercentage(column.getPercentageWidth(), columnIdentifer, "percentageWidth");
		validateResponsive(column.getResponsiveWidth(), columnIdentifer, "responsiveWidth");
		validateResponsive(column.getSm(), columnIdentifer, "sm");
		validateResponsive(column.getMd(), columnIdentifer, "md");
		validateResponsive(column.getLg(), columnIdentifer, "lg");
		validateResponsive(column.getXl(), columnIdentifer, "xl");
	}

	@Override
	public void visitFormRow(FormRow row, boolean parentVisible, boolean parentEnabled) {
		if (row.getItems().isEmpty()) {
			throw new MetaDataException("A form row in " + viewIdentifier + " is empty (has not items declared).");
		}
	}

	@Override
	public void visitFormItem(FormItem item, boolean parentVisible, boolean parentEnabled) {
		// TODO Implement FormItem validation
	}

	@Override
	public void visitGeometry(Geometry geometry, boolean parentVisible, boolean parentEnabled) {
		String geometryIdentifier = "Geometry " + geometry.getBinding();
		validateBinding(dataWidgetBinding,
							geometry.getBinding(),
							true,
							false,
							false,
							true,
							geometryIdentifier,
							AttributeType.geometry);
		validateConditionName(geometry.getDisabledConditionName(), geometryIdentifier);
		validateConditionName(geometry.getInvisibleConditionName(), geometryIdentifier);
		validateSize(geometry, geometryIdentifier);
	}

	@Override
	public void visitedGeometry(Geometry geometry, boolean parentVisible, boolean parentEnabled) {
		// nothing to validate
	}
	
	@Override
	public void visitGeometryMap(GeometryMap geometry, boolean parentVisible, boolean parentEnabled) {
		String geometryIdentifier = "GeometryMap " + geometry.getBinding();
		validateBinding(null,
							geometry.getBinding(),
							true,
							false,
							false,
							true,
							geometryIdentifier,
							AttributeType.geometry);
		validateConditionName(geometry.getDisabledConditionName(), geometryIdentifier);
		validateConditionName(geometry.getInvisibleConditionName(), geometryIdentifier);
		validateSize(geometry, geometryIdentifier);
	}

	@Override
	public void visitedGeometryMap(GeometryMap geometry, boolean parentVisible, boolean parentEnabled) {
		// nothing to validate
	}
	
	@Override
	public void visitMap(MapDisplay map, boolean parentVisible, boolean parentEnabled) {
		String geometryIdentifier = "Map with model " + map.getModelName();
		validateSize(map, geometryIdentifier);
		validateConditionName(map.getInvisibleConditionName(), geometryIdentifier);
		validateMapModelName(map.getModelName(), geometryIdentifier);
	}

	@Override
	public void visitChart(Chart chart, boolean parentVisible, boolean parentEnabled) {
		String modelName = chart.getModelName();
		ChartBuilderMetaData model = chart.getModel();
		String chartIdentifier = "Chart" + ((model == null) ? ((modelName == null) ? " with no model" : " with model named " + modelName) : " with model label " + model.getLabel());
		if (((modelName == null) && (model == null)) ||
				((modelName != null) && (model != null))) {
			throw new MetaDataException(chartIdentifier + " in " + viewIdentifier + " requires a modelName or a model but not both.");
		}
		validateSize(chart, chartIdentifier);
		validateConditionName(chart.getInvisibleConditionName(), chartIdentifier);
		if (modelName != null) {
			validateChartModelName(chart.getModelName(), chartIdentifier);
		}
		else {
			validateChartModel(model, chartIdentifier);
		}
	}
	
	private void validateChartModel(ChartBuilderMetaData model, String chartIdentifier) {
		Module contextModule = null;
		try {
			contextModule = repository.getModule(customer, model.getModuleName());
		}
		catch (Exception e) {
			throw new MetaDataException(chartIdentifier + " in " + viewIdentifier + " has an invalid moduleName of " + model.getModuleName(), e);
		}
		
		String documentName = model.getDocumentName();
		String queryName = model.getQueryName();

		Document contextDocument = null;
		if (documentName != null) {
			try {
				contextDocument = contextModule.getDocument(customer, documentName);
			}
			catch (Exception e) {
				throw new MetaDataException(chartIdentifier + " in " + viewIdentifier + " has an invalid documentName of " + documentName, e);
			}
		}
		else if (queryName != null) {
			MetaDataQueryDefinition query = contextModule.getMetaDataQuery(queryName);
			if (query == null) {
				throw new MetaDataException(chartIdentifier + " in " + viewIdentifier + " has an invalid queryName of " + queryName);
			}
			Module queryDocumentModule = query.getDocumentModule(customer);
			contextDocument = queryDocumentModule.getDocument(customer, query.getDocumentName());
		}
		else {
			throw new MetaDataException(chartIdentifier + " in " + viewIdentifier + " needs either a documentName or queryName");
		}

		String categoryBinding = model.getCategoryBinding();
		validateBinding(contextModule,
							contextDocument,
							null,
							categoryBinding,
							true,
							false,
							false,
							true,
							"The category binding of " + chartIdentifier);

		String valueBinding = model.getValueBinding();
		Class<?> type = validateBinding(contextModule,
											contextDocument,
											null,
											valueBinding,
											true,
											false,
											false,
											true,
											"The value binding of " + chartIdentifier);

		AggregateFunction function = model.getValueFunction();
		TargetMetaData target = BindUtil.getMetaDataForBinding(customer, contextModule, contextDocument, valueBinding);
		Attribute attribute = target.getAttribute();

		// check for numeric value if no value function is defined
		if (function == null) { // we need a number here
			if (attribute == null) { // implicit attribute
				throw new MetaDataException(chartIdentifier + " in " + viewIdentifier + 
												" has an invalid value binding of " + valueBinding + 
												" to an implicit attribute");
			}
			else if (! Number.class.isAssignableFrom(type)) {
				throw new MetaDataException(chartIdentifier + " in " + viewIdentifier + 
						" has an invalid value binding of " + valueBinding + 
						" to a non-numeric attribute");
				
			}
		}
		// check that aggregate function can be numeric, otherwise must be count
		else {
			if (! AggregateFunction.Count.equals(function)) {
				String invalidFunctionType = null;
				if (attribute == null) { // implicit attribute
					invalidFunctionType = "an implicit attribute";
				}
				else {
					if (! Number.class.isAssignableFrom(type)) {
						invalidFunctionType = "a non-numeric type of " + type;
					}
				}
				if (invalidFunctionType != null) {
					throw new MetaDataException(chartIdentifier + " in " + viewIdentifier + 
													" has an invalid valueFunction of " + function + 
													" for a value binding which is to " + invalidFunctionType);
				}
			}
		}
	}

	@Override
	public void visitHBox(HBox hbox, boolean parentVisible, boolean parentEnabled) {
		String borderTitle = hbox.getBorderTitle();
		String id = hbox.getWidgetId();
		String boxIdentifier = ((id == null) ? "A HBox" : "HBox " + id) + ((borderTitle == null) ? "" : " titled " + borderTitle);
		validateSize(hbox, boxIdentifier);
		validateConditionName(hbox.getInvisibleConditionName(), boxIdentifier);
		validateMessageExpressions(hbox.getLocalisedBorderTitle(), boxIdentifier, "borderTitle");
		validateCollapsible(hbox.getCollapsible(), hbox.getBorderTitle(), boxIdentifier);
	}

	@Override
	public void visitHTML(HTML html, boolean parentVisible, boolean parentEnabled) {
		String binding = html.getBinding();
		String htmlIdentifier = "HTML " + html.getBinding();
		if (dataWidgetBinding != null) {
			htmlIdentifier += " in " + dataWidgetIdentifier;
		}
		validateBinding(dataWidgetBinding,
							binding,
							true,
							false,
							false,
							true,
							htmlIdentifier,
							AttributeType.markup);
		validateConditionName(html.getDisabledConditionName(), htmlIdentifier);
		validateConditionName(html.getInvisibleConditionName(), htmlIdentifier);
		validateSize(html, htmlIdentifier);
	}

	@Override
	public void visitBlurb(Blurb blurb,
							boolean parentVisible,
							boolean parentEnabled) {
		String blurbIdentifier = "A Blurb";
		if (dataWidgetBinding != null) {
			blurbIdentifier += " in " + dataWidgetIdentifier;
		}
		String markup = blurb.getMarkup();
		if (markup == null) {
			throw new MetaDataException(blurbIdentifier + " in " + viewIdentifier + " has no markup specified.");
		}
		validateMessageExpressions(blurb.getLocalisedMarkup(), blurbIdentifier, "markup");
		validateConditionName(blurb.getInvisibleConditionName(), blurbIdentifier);
		validateSize(blurb, blurbIdentifier);
	}

	@Override
	public void visitLabel(Label label, boolean parentVisible, boolean parentEnabled) {
		String labelIdentifier = "A Label";
		if (dataWidgetBinding != null) {
			labelIdentifier += " in " + dataWidgetIdentifier;
		}

		validateBinding(dataWidgetBinding,
							label.getBinding(),
							false,
							false,
							false,
							true,
							labelIdentifier);
		validateBinding(dataWidgetBinding,
							label.getFor(),
							false,
							false,
							false,
							false,
							labelIdentifier);
		validateMessageExpressions(label.getLocalisedValue(), labelIdentifier, "a value");
		validateConditionName(label.getInvisibleConditionName(), labelIdentifier);
		validateSize(label, labelIdentifier);
	}

	@Override
	public void visitListGrid(ListGrid grid, boolean parentVisible, boolean parentEnabled) {
		String queryName = grid.getQueryName();
		String modelName = grid.getModelName();
		String listGridIdentifier = "ListGrid " + ((modelName != null) ? modelName : queryName);
		validateSize(grid, listGridIdentifier);
		validateConditionName(grid.getDisabledConditionName(), listGridIdentifier);
		validateConditionName(grid.getInvisibleConditionName(), listGridIdentifier);
		validateConditionName(grid.getDisableAddConditionName(), listGridIdentifier);
		validateConditionName(grid.getDisableEditConditionName(), listGridIdentifier);
		validateConditionName(grid.getDisableZoomConditionName(), listGridIdentifier);
		validateConditionName(grid.getDisableRemoveConditionName(), listGridIdentifier);
		validateConditionName(grid.getPostRefreshConditionName(), listGridIdentifier);
		validateBinding(null, grid.getSelectedIdBinding(), false, false, false, true, listGridIdentifier, AttributeType.id);
		Document drivingDocument = validateQueryOrModel(queryName, modelName, listGridIdentifier);
		validateFilterParameterBindings(grid.getFilterParameters(), listGridIdentifier, drivingDocument);
		validateParameterBindings(grid.getParameters(), listGridIdentifier);
		validateNoColonInFilterParameter(grid.getFilterParameters(), listGridIdentifier);
		validateNoColonInParameter(grid.getParameters(), listGridIdentifier);
	}

	@Override
	public void visitListRepeater(ListRepeater repeater, boolean parentVisible, boolean parentEnabled) {
		String queryName = repeater.getQueryName();
		String modelName = repeater.getModelName();
		String listRepeaterIdentifier = "ListRepeater " + ((modelName != null) ? modelName : queryName);
		validateSize(repeater, listRepeaterIdentifier);
		validateConditionName(repeater.getInvisibleConditionName(), listRepeaterIdentifier);
		validateConditionName(repeater.getPostRefreshConditionName(), listRepeaterIdentifier);
		Document drivingDocument = validateQueryOrModel(queryName, modelName, listRepeaterIdentifier);
		validateFilterParameterBindings(repeater.getFilterParameters(), listRepeaterIdentifier, drivingDocument);
		validateParameterBindings(repeater.getParameters(), listRepeaterIdentifier);
		validateNoColonInFilterParameter(repeater.getFilterParameters(), listRepeaterIdentifier);
		validateNoColonInParameter(repeater.getParameters(), listRepeaterIdentifier);
	}

	@Override
	public void visitTreeGrid(TreeGrid grid, boolean parentVisible, boolean parentEnabled) {
		String queryName = grid.getQueryName();
		String modelName = grid.getModelName();
		String treeGridIdentifier = "TreeGrid " + ((modelName != null) ? modelName : queryName);
		validateSize(grid, treeGridIdentifier);
		validateConditionName(grid.getDisabledConditionName(), treeGridIdentifier);
		validateConditionName(grid.getInvisibleConditionName(), treeGridIdentifier);
		validateConditionName(grid.getDisableAddConditionName(), treeGridIdentifier);
		validateConditionName(grid.getDisableEditConditionName(), treeGridIdentifier);
		validateConditionName(grid.getDisableZoomConditionName(), treeGridIdentifier);
		validateConditionName(grid.getDisableRemoveConditionName(), treeGridIdentifier);
		validateConditionName(grid.getPostRefreshConditionName(), treeGridIdentifier);
		validateBinding(null, grid.getSelectedIdBinding(), false, false, false, true, treeGridIdentifier, AttributeType.id);
		validateBinding(null, grid.getRootIdBinding(), false, false, false, true, treeGridIdentifier);
		Document drivingDocument = validateQueryOrModel(queryName, modelName, treeGridIdentifier);
		validateFilterParameterBindings(grid.getFilterParameters(), treeGridIdentifier, drivingDocument);
		validateParameterBindings(grid.getParameters(), treeGridIdentifier);
		validateNoColonInFilterParameter(grid.getFilterParameters(), treeGridIdentifier);
		validateNoColonInParameter(grid.getParameters(), treeGridIdentifier);
	}

	@Override
	public void visitListMembership(ListMembership membership, boolean parentVisible, boolean parentEnabled) {
		String membershipIdentifier = "ListMembership " + membership.getBinding();
		validateBinding(null,
							membership.getBinding(),
							true,
							false,
							true,
							false,
							membershipIdentifier,
							AttributeType.collection,
							AttributeType.inverseMany);
		validateConditionName(membership.getDisabledConditionName(), membershipIdentifier);
		validateConditionName(membership.getInvisibleConditionName(), membershipIdentifier);
		validateSize(membership, membershipIdentifier);
	}

	@Override
	public void visitedListMembership(ListMembership membership,
										boolean parentVisible,
										boolean parentEnabled) {
		// do nothing
	}
	
	@Override
	public void visitComparison(Comparison comparison,
									boolean parentVisible,
									boolean parentEnabled) {
		String comparisonIdentifier = "Comparison " + comparison.getBinding();
		validateBinding(null,
							comparison.getBinding(),
							true,
							true,
							false,
							false,
							comparisonIdentifier,
							AttributeType.association);
		validateConditionName(comparison.getDisabledConditionName(), comparisonIdentifier);
		validateConditionName(comparison.getInvisibleConditionName(), comparisonIdentifier);
		validateComparisonModelName(comparison.getModelName(), comparisonIdentifier);
		validateSize(comparison, comparisonIdentifier);
	}

	@Override
	public void visitLookupDescription(LookupDescription lookup, boolean parentVisible, boolean parentEnabled) {
		String binding = lookup.getBinding();
		String descriptionBinding = lookup.getDescriptionBinding();
		String lookupIdentifier = "LookupDescription " + binding;
		validateSize(lookup, lookupIdentifier);
		// A lookupDescription in a data grid bound to an aggregated collection 
		// doesn't have to have a binding
		validateBinding(dataWidgetBinding,
							binding,
							(dataWidgetBinding == null),
							false,
							false,
							false,
							lookupIdentifier,
							AttributeType.association,
							AttributeType.inverseOne);
		validateBinding(dataWidgetBinding,
							// binding can be null if dataGridBinding is set and this 
							// is a lookup to the elements in the collection
							(binding == null) ? descriptionBinding : BindUtil.createCompoundBinding(binding, descriptionBinding),
							true,
							false,
							false,
							true,
							lookupIdentifier);
		validateConditionName(lookup.getDisabledConditionName(), lookupIdentifier);
		validateConditionName(lookup.getInvisibleConditionName(), lookupIdentifier);
		validateConditionName(lookup.getDisablePickConditionName(), lookupIdentifier);
		validateConditionName(lookup.getDisableEditConditionName(), lookupIdentifier);
		validateConditionName(lookup.getDisableAddConditionName(), lookupIdentifier);
		validateConditionName(lookup.getDisableClearConditionName(), lookupIdentifier);
		Document drivingDocument = validateQueryName(lookup.getQuery(), lookupIdentifier);
		validateFilterParameterBindings(lookup.getFilterParameters(), lookupIdentifier, drivingDocument);
		validateParameterBindings(lookup.getParameters(), lookupIdentifier);
		validateNoColonInFilterParameter(lookup.getFilterParameters(), lookupIdentifier);
		validateNoColonInParameter(lookup.getParameters(), lookupIdentifier);
		
		// determine the query that will be used
		MetaDataQueryDefinition query = null;
		if (lookup.getQuery() != null) {
    		query = module.getMetaDataQuery(lookup.getQuery());
    	}
		else {
			// NB Use getMetaDataForBinding() to ensure we find attributes from base documents inherited
			String fullBinding = binding;
			if (dataWidgetBinding != null) {
				if (binding == null) {
					fullBinding = dataWidgetBinding;
				}
				else {
					fullBinding = BindUtil.createCompoundBinding(dataWidgetBinding, binding);
				}
			}
			TargetMetaData target = Binder.getMetaDataForBinding(customer, module, document, fullBinding);
    		Relation relation = (Relation) target.getAttribute();
    		// This should never happen as shit was validated above
    		if (relation == null) {
    			throw new MetaDataException(fullBinding + " doesn't point to an attribute from document " + document.getName());
    		}
    		String queryName = (relation instanceof Reference) ? ((Reference) relation).getQueryName() : null;
    		if (queryName != null) {
        		query = module.getMetaDataQuery(queryName);
    		}
    		else {
    			query = module.getDocumentDefaultQuery(customer, relation.getDocumentName());
    		}
		}

		// validate drop down columns and description binding
		List<LookupDescriptionColumn> dropDownColumns = lookup.getDropDownColumns();
		LinkedHashSet<String> testColumns = null;
		if (dropDownColumns != null) {
			testColumns = new LinkedHashSet<>(dropDownColumns.size());
			for (LookupDescriptionColumn dropDownColumn : dropDownColumns) {
				String dropDownColumnName = dropDownColumn.getName();
				if (! Bean.BIZ_KEY.equals(dropDownColumnName)) {
					testColumns.add(dropDownColumnName);
				}
			}
		}
		boolean foundLookupDescription = Bean.BIZ_KEY.equals(descriptionBinding);
		
		for (MetaDataQueryColumn column : query.getColumns()) {
    		String alias = column.getName();
    		if (alias == null) {
    			alias = column.getBinding();
    		}
    		MetaDataQueryProjectedColumn projectedColumn = (column instanceof MetaDataQueryProjectedColumn) ? 
    															(MetaDataQueryProjectedColumn) column : 
																null;
    		if ((testColumns != null) && testColumns.contains(alias)) {
        		if ((projectedColumn != null) && (! projectedColumn.isProjected())) {
					throw new MetaDataException(lookupIdentifier + " in " + viewIdentifier + " has a drop down column of " + alias + " which is not projected in the query.");
        		}
        		testColumns.remove(alias);
            }
            if ((! foundLookupDescription) && descriptionBinding.equals(alias)) {
        		if ((projectedColumn != null) && (! projectedColumn.isProjected())) {
        			throw new MetaDataException(lookupIdentifier + " in " + viewIdentifier + " has a description binding of " + alias + " which is not projected in the query.");
        		}
            	foundLookupDescription = true;
            }
    	}
		
    	if (! foundLookupDescription) {
			throw new MetaDataException(lookupIdentifier + " in " + viewIdentifier + " has a description binding of " + descriptionBinding + " which is not defined in the query.");
    	}
    	if ((testColumns != null) && (! testColumns.isEmpty())) {
			throw new MetaDataException(lookupIdentifier + " in " + viewIdentifier + " has a drop down column of " + testColumns.iterator().next() + " which is not defined in the query.");
    	}
	}

	@Override
	public void visitedLookupDescription(LookupDescription lookup, boolean parentVisible, boolean parentEnabled) {
		// do nothing
	}

	@Override
	public void visitParameter(Parameter parameter, boolean parentVisible, boolean parentEnabled) {
		// Check that value or valueBinding is populated
		if ((parameter.getValue() == null) && (parameter.getValueBinding() == null)) {
			throw new MetaDataException("Parameter " + parameter.getName() + " requires either a value or a valueBinding.");
		}
	}

	@Override
	public void visitFilterParameter(FilterParameter parameter, boolean parentVisible, boolean parentEnabled) {
		// Check that value or valueBinding is populated for pertinent operators
		FilterOperator operator = parameter.getOperator();
		if ((! FilterOperator.isNull.equals(operator)) &&
				(! FilterOperator.notNull.equals(operator)) &&
				(parameter.getValue() == null) &&
				(parameter.getValueBinding() == null)) {
			throw new MetaDataException("Filter Parameter " + parameter.getFilterBinding() + " requires either a value or a valueBinding for operator " + parameter.getOperator());
		}
	}

	@Override
	public void visitPassword(Password password, boolean parentVisible, boolean parentEnabled) {
		String binding = password.getBinding();
		String passwordIdentifier = "Password " + binding;
		if (dataWidgetBinding != null) {
			passwordIdentifier += " in " + dataWidgetIdentifier;
		}
		validateBinding(dataWidgetBinding,
							binding,
							true,
							false,
							false,
							true,
							passwordIdentifier,
							AttributeType.text);
		validateConditionName(password.getDisabledConditionName(), passwordIdentifier);
		validateConditionName(password.getInvisibleConditionName(), passwordIdentifier);
		validateSize(password, passwordIdentifier);
	}

	@Override
	public void visitedPassword(Password password,
									boolean parentVisible,
									boolean parentEnabled) {
		// do nothing
	}

	@Override
	public void visitProgressBar(ProgressBar progressBar, boolean parentVisible, boolean parentEnabled) {
		String progressBarIdentifier = "ProgressBar " + progressBar.getBinding();
		validateBinding(null,
							progressBar.getBinding(),
							true,
							false,
							false,
							true,
							progressBarIdentifier);
		validateConditionName(progressBar.getInvisibleConditionName(), progressBarIdentifier);
		validateSize(progressBar, progressBarIdentifier);
	}

	@Override
	public void visitRadio(Radio radio, boolean parentVisible, boolean parentEnabled) {
		String binding = radio.getBinding();
		String radioIdentifier = "Radio " + binding;
		if (dataWidgetBinding != null) {
			radioIdentifier += " in " + dataWidgetIdentifier;
		}
		validateBinding(dataWidgetBinding,
							binding,
							true,
							false,
							true,
							false,
							radioIdentifier,
							AttributeType.enumeration,
							AttributeType.text,
							AttributeType.association,
							AttributeType.inverseOne,
							AttributeType.bool,
							AttributeType.integer,
							AttributeType.longInteger);
		validateConditionName(radio.getDisabledConditionName(), radioIdentifier);
		validateConditionName(radio.getInvisibleConditionName(), radioIdentifier);
		validateSize(radio, radioIdentifier);
	}

	@Override
	public void visitedRadio(Radio radio,
								boolean parentVisible,
								boolean parentEnabled) {
		// do nothing
	}

	@Override
	public void visitRichText(RichText richText, boolean parentVisible, boolean parentEnabled) {
		String binding = richText.getBinding();
		String richTextIdentifier = "RichText " + binding;
		if (dataWidgetBinding != null) {
			richTextIdentifier += " in " + dataWidgetIdentifier;
		}
		validateBinding(dataWidgetBinding,
							binding,
							true,
							false,
							false,
							true,
							richTextIdentifier,
							AttributeType.memo,
							AttributeType.markup);
		validateConditionName(richText.getDisabledConditionName(), richTextIdentifier);
		validateConditionName(richText.getInvisibleConditionName(), richTextIdentifier);
		validateSize(richText, richTextIdentifier);
	}

	@Override
	public void visitedRichText(RichText richText,
									boolean parentVisible,
									boolean parentEnabled) {
		// do nothing
	}

	@Override
	public void visitSlider(Slider slider, boolean parentVisible, boolean parentEnabled) {
		String binding = slider.getBinding();
		String sliderIdentifier = "Slider " + binding;
		if (dataWidgetBinding != null) {
			sliderIdentifier += " in " + dataWidgetBinding;
		}
		validateBinding(dataWidgetBinding,
							binding,
							true,
							false,
							false,
							true,
							sliderIdentifier);
		validateConditionName(slider.getDisabledConditionName(), sliderIdentifier);
		validateConditionName(slider.getInvisibleConditionName(), sliderIdentifier);
		validateSize(slider, sliderIdentifier);
	}

	@Override
	public void visitedSlider(Slider slider,
								boolean parentVisible,
								boolean parentEnabled) {
		// do nothing
	}

	@Override
	public void visitSpacer(Spacer spacer) {
		String spacerIdentifier = "A Spacer";
		validateConditionName(spacer.getInvisibleConditionName(), spacerIdentifier);
		validateSize(spacer, spacerIdentifier);
	}

	@Override
	public void visitSpinner(Spinner spinner, boolean parentVisible, boolean parentEnabled) {
		String binding = spinner.getBinding();
		String spinnerIdentifier = "Spinner " + binding;
		if (dataWidgetBinding != null) {
			spinnerIdentifier += " in " + dataWidgetBinding;
		}
		validateBinding(dataWidgetBinding,
							binding,
							true,
							false,
							false,
							true,
							spinnerIdentifier);
		validateConditionName(spinner.getDisabledConditionName(), spinnerIdentifier);
		validateConditionName(spinner.getInvisibleConditionName(), spinnerIdentifier);
		validateSize(spinner, spinnerIdentifier);
	}

	@Override
	public void visitedSpinner(Spinner spinner,
								boolean parentVisible,
								boolean parentEnabled) {
		// do nothing
	}

	@Override
	public void visitStaticImage(StaticImage image, boolean parentVisible, boolean parentEnabled) {
		String imageIdentifier = "StaticImage " + image.getRelativeFile();
		validateConditionName(image.getInvisibleConditionName(), imageIdentifier);
		validateSize(image, imageIdentifier);
	}

	@Override
	public void visitLink(Link link, boolean parentVisible, boolean parentEnabled) {
		final String linkIdentifier = "Link " + link.getValue();
		validateConditionName(link.getInvisibleConditionName(), linkIdentifier);
		validateSize(link, linkIdentifier);

		new ReferenceProcessor() {
			@SuppressWarnings("synthetic-access")
			private ModuleImpl validateReferenceModuleName(String referenceModuleName, 
															String referenceDescription) {
				ModuleImpl result = null;
				
				if (referenceModuleName.indexOf('{') < 0) {
					try {
						result = (ModuleImpl) repository.getModule(customer, referenceModuleName);
						if (result == null) {
							throw new MetaDataException(referenceModuleName + " DNE");
						}
					}
					catch (Exception e) {
						throw new MetaDataException(linkIdentifier + " in " + 
														viewIdentifier + " has " + 
														referenceDescription + " reference with an invalid module of " + 
														referenceModuleName, e);
					}
				}
				
				return result;
			}
			
			@SuppressWarnings("synthetic-access")
			private DocumentImpl validateReferenceDocumentName(ModuleImpl referenceModule,
															String referenceDocumentName,
															String referenceDescription) {
				DocumentImpl result = null;
				
				if (referenceDocumentName.indexOf('{') < 0) {
					try {
						result = (DocumentImpl) referenceModule.getDocument(customer, referenceDocumentName);
						if (result == null) {
							throw new MetaDataException(referenceDocumentName + " DNE");
						}
						return result;
					}
					catch (Exception e) {
						throw new MetaDataException(linkIdentifier + " in " + 
														viewIdentifier + " has " + 
														referenceDescription + " reference with an invalid document of " + 
														referenceModule.getName() + '.' + referenceDocumentName, e);
					}
				}
				
				return result;
			}
			
			@SuppressWarnings("synthetic-access")
			private TargetMetaData validateReferenceBinding(String referenceBinding,
																String referenceDescription) {
				String bindingToTest = referenceBinding;
				if (dataWidgetBinding != null) {
					if (referenceBinding == null) {
						bindingToTest = dataWidgetBinding;
					}
					else {
						bindingToTest = BindUtil.createCompoundBinding(dataWidgetBinding, referenceBinding);
					}
				}

				TargetMetaData result = null;
				// NB bindingToTest can be null when a link to an edit view for a new document instance
				// and the link is NOT in a grid container column
				if (bindingToTest != null) {
					try {
						result = BindUtil.getMetaDataForBinding(customer, module, document, bindingToTest);
						if (result == null) {
							throw new IllegalStateException("Target DNE");
						}
						return result;
					}
					catch (MetaDataException e) {
						throw new MetaDataException(linkIdentifier + " in " + viewIdentifier + 
														" has " + referenceDescription + 
														" reference with an invalid binding of " + referenceBinding, e);
					}
				}
				
				return result;
			}
			
			@Override
			public void processResourceReference(ResourceReference reference) {
				// nothing to do here
			}
			
			@Override
			@SuppressWarnings("synthetic-access")
			public void processReportReference(ReportReference reference) {
				ModuleImpl reportModule = validateReferenceModuleName(reference.getModuleName(),
																	"a report");
				if (reportModule != null) { // valid module name with no '{'
					DocumentImpl reportDocument = validateReferenceDocumentName(reportModule,
																				reference.getDocumentName(),
																				"a report");
					if (reportDocument != null) { // valid module name with no '{'
						try {
							if (repository.getReportFileName(customer, reportDocument, reference.getReportName()) == null) {
								throw new IllegalStateException("Report DNE");
							}
						}
						catch (Exception e) { // could be NPE or IllegalArgument etc etc
							throw new MetaDataException(linkIdentifier + " in " + viewIdentifier + 
															" has a report reference with an invalid report name of " + 
															reportModule.getName() + '.' + 
															reportDocument.getName() + '.' +
															reference.getReportName(), e);
						}
					}
				}
				validateParameterBindings(reference.getParameters(), linkIdentifier);
			}
			
			@Override
			@SuppressWarnings("synthetic-access")
			public void processQueryListViewReference(QueryListViewReference reference) {
				try {
					if (module.getMetaDataQuery(reference.getQueryName()) == null) {
						throw new IllegalStateException("No such query");
					}
				}
				catch (Exception e) {
					throw new MetaDataException(linkIdentifier + " in " + viewIdentifier + 
													" has a query list view reference with an invalid query name of " + 
													reference.getQueryName(), e);
				}
			}
			
			@Override
			public void processImplicitActionReference(ImplicitActionReference reference) {
				// nothing to do here
			}
			
			@Override
			public void processExternalReference(ExternalReference reference) {
				// nothing to do here
			}
			
			@Override
			public void processEditViewReference(EditViewReference reference) {
				ModuleImpl viewModule = validateReferenceModuleName(reference.getModuleName(), "an edit view");
				if (viewModule != null) { // valid module name with no '{'
					validateReferenceDocumentName(viewModule, 
													reference.getDocumentName(),
													"an edit view");
				}
				validateReferenceBinding(reference.getBinding(), "an edit view");
			}
			
			@Override
			public void processDefaultListViewReference(DefaultListViewReference reference) {
				ModuleImpl viewModule = validateReferenceModuleName(reference.getModuleName(), "an edit view");
				if (viewModule != null) { // valid module name with no '{'
					validateReferenceDocumentName(viewModule, 
													reference.getDocumentName(),
													"an edit view");
				}
			}
			
			@Override
			public void processContentReference(ContentReference reference) {
				String widgetidentifier = linkIdentifier + " with a content reference";
				if (dataWidgetBinding != null) {
					widgetidentifier += " in " + dataWidgetIdentifier;
				}
				validateBinding(dataWidgetBinding,
									reference.getBinding(),
									true,
									false,
									false,
									true,
									widgetidentifier,
									AttributeType.content);
			}
			
			@Override
			@SuppressWarnings("synthetic-access")
			public void processActionReference(ActionReference reference) {
				String widgetIdentifier = linkIdentifier + " with an action reference";
				if (dataWidgetBinding != null) { // in a repeater or grid
					widgetIdentifier += " in " + dataWidgetBinding;
					String actionName = reference.getActionName();
					try {
						TargetMetaData target = validateReferenceBinding(null, "an action reference");
						Reference targetReference = (Reference) target.getAttribute();
						if (targetReference == null) {
							throw new MetaDataException("Target Reference " + dataWidgetBinding + " DNE");
						}
						ModuleImpl targetModule = (ModuleImpl) repository.getModule(customer, target.getDocument().getOwningModuleName());
						DocumentImpl targetDocument = (DocumentImpl) targetModule.getDocument(customer, targetReference.getDocumentName());
						
						// This is a container column of an existing row in a table/grid - so get the edit view
						ViewImpl targetView = (ViewImpl) targetDocument.getView(currentUxUi, customer, ViewType.edit.toString());
						if (targetView.getAction(actionName) == null) {
							throw new MetaDataException(actionName + " DNE");
						}
					}
					catch (Exception e) {
						throw new MetaDataException(widgetIdentifier + " in " + viewIdentifier + " references a non-existent action " + actionName, e);
					}
				}
				else {
					validateActionName(reference.getActionName(), widgetIdentifier);
				}
			}
		}.process(link.getReference());
	}

	@Override
	public void visitTab(Tab tab, boolean parentVisible, boolean parentEnabled) {
		String tabIdentifier = "Tab " + tab.getTitle();
		validateConditionName(tab.getDisabledConditionName(), tabIdentifier);
		validateConditionName(tab.getInvisibleConditionName(), tabIdentifier);
		validateMessageExpressions(tab.getLocalisedTitle(), tabIdentifier, "title");
	}

	@Override
	public void visitTabPane(TabPane tabPane, boolean parentVisible, boolean parentEnabled) {
		String tabPaneIdentifier = tabPane.getWidgetId();
		if (tabPaneIdentifier != null) {
			tabPaneIdentifier = "TabPane " + tabPaneIdentifier;
		}
		else {
			tabPaneIdentifier = "A TabPane";
		}
		validateSize(tabPane, tabPaneIdentifier);
		validateConditionName(tabPane.getDisabledConditionName(), tabPaneIdentifier);
		validateConditionName(tabPane.getInvisibleConditionName(), tabPaneIdentifier);
		validateBinding(null, 
							tabPane.getSelectedTabIndexBinding(), 
							false, 
							false, 
							false, 
							true, 
							"The [selectedTabIndexBinding] of " + tabPaneIdentifier, 
							AttributeType.integer, 
							AttributeType.longInteger);
	}

	@Override
	public void visitTextArea(TextArea text, boolean parentVisible, boolean parentEnabled) {
		String binding = text.getBinding();
		String textIdentifier = "TextArea " + binding;
		if (dataWidgetBinding != null) {
			textIdentifier += " in " + dataWidgetBinding;
		}
		validateBinding(dataWidgetBinding,
							binding,
							true,
							false,
							false,
							true,
							textIdentifier);
		validateConditionName(text.getDisabledConditionName(), textIdentifier);
		validateConditionName(text.getInvisibleConditionName(), textIdentifier);
		validateSize(text, textIdentifier);
	}

	@Override
	public void visitedTextArea(TextArea text,
									boolean parentVisible,
									boolean parentEnabled) {
		// do nothing
	}

	@Override
	public void visitTextField(TextField text, boolean parentVisible, boolean parentEnabled) {
		String binding = text.getBinding();
		String textIdentifier = "Text " + binding;
		if (dataWidgetBinding != null) {
			textIdentifier += " in " + dataWidgetBinding;
		}
		validateBinding(dataWidgetBinding,
							binding,
							true,
							false,
							false,
							true,
							textIdentifier);
		validateConditionName(text.getDisabledConditionName(), textIdentifier);
		validateConditionName(text.getInvisibleConditionName(), textIdentifier);
		validateSize(text, textIdentifier);
	}

	@Override
	public void visitedTextField(TextField text,
									boolean parentVisible,
									boolean parentEnabled) {
		// do nothing
	}

	
	@Override
	public void visitInject(Inject inject,
								boolean parentVisible,
								boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitVBox(VBox vbox, boolean parentVisible, boolean parentEnabled) {
		String borderTitle = vbox.getBorderTitle();
		String id = vbox.getWidgetId();
		String boxIdentifier = ((id == null) ? "A VBox" : "VBox " + id) + ((borderTitle == null) ? "" : " titled " + borderTitle);
		validateSize(vbox, boxIdentifier);
		validateConditionName(vbox.getInvisibleConditionName(), boxIdentifier);
		validateMessageExpressions(vbox.getLocalisedBorderTitle(), boxIdentifier, "borderTitle");
		validateCollapsible(vbox.getCollapsible(), vbox.getBorderTitle(), boxIdentifier);
	}

	@Override
	public void visitView() {
		validateMessageExpressions(view.getLocalisedTitle(), viewIdentifier, "a title");

		List<ViewParameter> parameters = view.getParameters();
		if (parameters != null) {
			for (ViewParameter parameter : parameters) {
				validateBinding(null,
									parameter.getBoundTo(),
									false,
									false,
									false,
									false,
									"Parameter " + parameter.getFromBinding() + " in " + viewIdentifier);
			}
		}
		
		validateActionName(view.getRefreshActionName(), viewIdentifier);
		validateConditionName(view.getRefreshConditionName(), viewIdentifier);
		if ((view.getHelpURL() != null) && (view.getHelpRelativeFileName() != null)) {
			throw new MetaDataException(viewIdentifier + " should define one of helpURL or helpRelativeFileName but not both");
		}
	}

	@Override
	public void visitedDataGrid(DataGrid grid, boolean parentVisible, boolean parentEnabled) {
		dataWidgetBinding = null;
		dataWidgetIdentifier = null;
	}

	@Override
	public void visitedDataRepeater(DataRepeater repeater, boolean parentVisible, boolean parentEnabled) {
		dataWidgetBinding = null;
		dataWidgetIdentifier = null;
	}

	@Override
	public void visitedForm(Form form, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitedFormItem(FormItem item, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitedFormRow(FormRow row, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitedHBox(HBox hbox, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitedListGrid(ListGrid grid, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitedListRepeater(ListRepeater repeater, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitedTreeGrid(TreeGrid grid, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitedTab(Tab tab, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitedTabPane(TabPane tabPane, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitedVBox(VBox vbox, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitedView() {
		// nothing to do here
	}

	private void validateAction(ActionImpl action) {
		String actionIdentifier = "Action " + action.getName();
		
		validateConditionName(action.getDisabledConditionName(), actionIdentifier);
		validateConditionName(action.getInvisibleConditionName(), actionIdentifier);
		validateParameterBindings(action.getParameters(), actionIdentifier);
		
		if ((ActionShow.icon == action.getShow()) && 
				(action.getIconStyleClass() == null) &&
				(action.getRelativeIconFileName() == null)) {
			throw new MetaDataException(actionIdentifier + " in " + viewIdentifier + " is set to [show] an icon but has no [iconStyleClass] or [relativeIconFileName] defined.");
		}
	}
	
	// validate the resource name which represents the class to load for ClassActions
	private void validateClassAction(String resourceName) {
		if (ImplicitActionName.Print.toString().equals(resourceName)) {
			throw new MetaDataException("Name clash: Action " + resourceName + " is an implicit action name and cannot be used.");
		}
		StringBuilder key = new StringBuilder(128);
		key.append(ProvidedRepository.MODULES_NAMESPACE).append(document.getOwningModuleName()).append('/');
		key.append(document.getName()).append('/');
		key.append(ProvidedRepository.ACTIONS_NAMESPACE).append(resourceName);
		if (repository.getJavaClass(customer, key.toString()) == null) {
			throw new MetaDataException(key + " not found.");
		}
	}
	
	@Override
	public void visitCustomAction(ActionImpl action) {
		String resourceName = action.getResourceName();
		ActionMetaData metaDataAction = repository.getMetaDataAction(customer, document, resourceName);
		if (metaDataAction == null) {
			validateClassAction(resourceName);
		}
		validateAction(action);
	}

	@Override
	public void visitAddAction(ActionImpl action) {
		validateAction(action);
	}

	@Override
	public void visitBizExportAction(ActionImpl action) {
		validateClassAction(action.getResourceName());
		validateAction(action);
	}

	@Override
	public void visitBizImportAction(ActionImpl action) {
		validateClassAction(action.getResourceName());
		validateAction(action);
	}

	@Override
	public void visitDownloadAction(ActionImpl action) {
		validateClassAction(action.getResourceName());
		validateAction(action);
	}

	@Override
	public void visitUploadAction(ActionImpl action) {
		validateClassAction(action.getResourceName());
		validateAction(action);
	}

	@Override
	public void visitCancelAction(ActionImpl action) {
		validateAction(action);
	}

	@Override
	public void visitDeleteAction(ActionImpl action) {
		validateAction(action);
	}

	@Override
	public void visitEditAction(ActionImpl action) {
		validateAction(action);
	}

	@Override
	public void visitNavigateAction(ActionImpl action) {
		validateAction(action);
	}

	@Override
	public void visitNewAction(ActionImpl action) {
		validateAction(action);
	}

	@Override
	public void visitOKAction(ActionImpl action) {
		validateAction(action);
	}

	@Override
	public void visitRemoveAction(ActionImpl action) {
		validateAction(action);
	}

	@Override
	public void visitReportAction(ActionImpl action) {
		validateAction(action);

		List<Parameter> reportParameters = action.getParameters();

		// Find the moduleName, documentName and reportFormat parameter values
		String moduleName = null;
		String documentName = null;
		String reportFormat = null;
		
		for (Parameter reportParameter : reportParameters) {
			String name = reportParameter.getName();
			if (AbstractWebContext.REPORT_FORMAT.equals(name)) {
				reportFormat = reportParameter.getValue();
			}
			else if (AbstractWebContext.MODULE_NAME.equals(name)) {
				moduleName = reportParameter.getValue();
			}
			else if (AbstractWebContext.DOCUMENT_NAME.equals(name)) {
				documentName = reportParameter.getValue();
			}
		}
		
		Document reportDocument = null;
		try {
			reportDocument = customer.getModule(moduleName).getDocument(customer, documentName);
		}
		catch (Exception e) {
			throw new MetaDataException("Report Action for report " + action.getResourceName() + 
											" in view " + viewIdentifier + 
											" does not reference a valid document of " + 
											moduleName + "." + documentName,
											e);
		}
		
		String fileName = repository.getReportFileName(customer, reportDocument, action.getResourceName());
		if (fileName == null) {
			throw new MetaDataException("Report Action for report " + action.getResourceName() + 
											" in view " + viewIdentifier + 
											" does not reference a Jasper or Freemarker resource");
		}

		ParameterImpl parameter = new ParameterImpl();
		parameter.setName(AbstractWebContext.REPORT_ENGINE);
		if (fileName.endsWith(".jasper")) {
			parameter.setValue(ProvidedRepository.JASPER_SUFFIX);
		}
		else if (fileName.endsWith(".flth")) {
			parameter.setValue(ProvidedRepository.FREEMARKER_SUFFIX);
			
			// Report format can on be PDF or CSV for freemarker
			if (reportFormat != null) {
				if (! (ReportFormat.pdf.toString().equals(reportFormat) || ReportFormat.csv.toString().equals(reportFormat))) {
					throw new MetaDataException("Report Action for report " + action.getResourceName() + 
													" in view " + viewIdentifier + 
													" has a format of " + reportFormat + 
													" but must be PDF or CSV for a freemarker report");
				}
			}
		}
		else {
			throw new MetaDataException("Report action for report " + action.getResourceName() + 
											" in view " + viewIdentifier + 
											" does not reference a Jasper or Freemarker resource");
		}
		reportParameters.add(parameter);
	}

	@Override
	public void visitSaveAction(ActionImpl action) {
		validateAction(action);
	}

	@Override
	public void visitZoomOutAction(ActionImpl action) {
		validateAction(action);
	}

	private static void validateEventHandlerSequence(List<EventAction> actions, String widgetIdentifier) {
		if (actions != null) {
			Iterator<EventAction> i = actions.iterator();
			while (i.hasNext()) {
				EventAction action = i.next();
				if (i.hasNext()) {
					if (action instanceof RerenderEventAction) {
						throw new MetaDataException("[rerender] event action in " +  widgetIdentifier +
														" has to be the last action as it is a server-side action.");
					}
					else if (action instanceof ServerSideActionEventAction) {
						throw new MetaDataException("[server] event action to action " +
														((ServerSideActionEventAction) action).getActionName() +
														" in " +  widgetIdentifier +
														" has to be the last action as it is a server-side action.");
					}
				}
			}
		}
	}
	
	@Override
	public void visitOnChangedEventHandler(Changeable changeable,
											boolean parentVisible,
											boolean parentEnabled) {
		validateEventHandlerSequence(changeable.getChangedActions(),
										"[onChanged] event handler for widget with binding " + 
											changeable.getBinding());
	}

	@Override
	public void visitedOnChangedEventHandler(Changeable changeable,
												boolean parentVisible,
												boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitOnFocusEventHandler(Focusable focusable,
											boolean parentVisible,
											boolean parentEnabled) {
		String binding = (focusable instanceof Bound) ? ((Bound) focusable).getBinding() : "unknown";
		validateEventHandlerSequence(focusable.getFocusActions(),
				"[onFocus] event handler for widget with binding " + binding);
	}

	@Override
	public void visitedOnFocusEventHandler(Focusable blurable,
											boolean parentVisible,
											boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitOnBlurEventHandler(Focusable focusable,
											boolean parentVisible,
											boolean parentEnabled) {
		String binding = (focusable instanceof Bound) ? ((Bound) focusable).getBinding() : "unknown";
		validateEventHandlerSequence(focusable.getBlurActions(),
				"[onBlur] event handler for widget with binding " + binding);
	}

	@Override
	public void visitedOnBlurEventHandler(Focusable blurable,
											boolean parentVisible,
											boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitOnAddedEventHandler(Addable addable,
											boolean parentVisible,
											boolean parentEnabled) {
		String widgetIdentifier = "Unknown widget";
		if (addable instanceof Bound) {
			widgetIdentifier = "[onAdded] event handler for widget with binding " + ((Bound) addable).getBinding();
		}
		else if (addable instanceof ListGrid) {
			widgetIdentifier = "[onAdded] event handler for list grid with query " + ((ListGrid) addable).getQueryName();
		}
		validateEventHandlerSequence(addable.getAddedActions(), widgetIdentifier);
	}

	@Override
	public void visitedOnAddedEventHandler(Addable addable,
											boolean parentVisible,
											boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitOnEditedEventHandler(Editable editable,
											boolean parentVisible,
											boolean parentEnabled) {
		String widgetIdentifier = "Unknown widget";
		if (editable instanceof Bound) {
			widgetIdentifier = "[onEdited] event handler for widget with binding " + ((Bound) editable).getBinding();
		}
		else if (editable instanceof ListGrid) {
			widgetIdentifier = "[onEdited] event handler for list grid with query " + ((ListGrid) editable).getQueryName();
		}
		validateEventHandlerSequence(editable.getEditedActions(), widgetIdentifier);
	}

	@Override
	public void visitedOnEditedEventHandler(Editable editable,
												boolean parentVisible,
												boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitOnRemovedEventHandler(Removable removable,
											boolean parentVisible,
											boolean parentEnabled) {
		String widgetIdentifier = "Unknown widget";
		if (removable instanceof Bound) {
			widgetIdentifier = "[onRemoved] event handler for widget with binding " + ((Bound) removable).getBinding();
		}
		else if (removable instanceof ListGrid) {
			widgetIdentifier = "[onRemoved] event handler for list grid with query " + ((ListGrid) removable).getQueryName();
		}
		validateEventHandlerSequence(removable.getRemovedActions(), widgetIdentifier);
	}

	@Override
	public void visitedOnRemovedEventHandler(Removable removable,
												boolean parentVisible,
												boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitOnSelectedEventHandler(Selectable selectable,
												boolean parentVisible,
												boolean parentEnabled) {
		String widgetIdentifier = "Unknown widget";
		if (selectable instanceof Bound) {
			widgetIdentifier = "[onSelected] event handler for widget with binding " + ((Bound) selectable).getBinding();
		}
		else if (selectable instanceof ListGrid) {
			widgetIdentifier = "[onSelected] event handler for list grid with query " + ((ListGrid) selectable).getQueryName();
		}
		validateEventHandlerSequence(selectable.getSelectedActions(), widgetIdentifier);
	}

	@Override
	public void visitedOnSelectedEventHandler(Selectable editable,
												boolean parentVisible,
												boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitOnPickedEventHandler(LookupDescription lookup,
											boolean parentVisible,
											boolean parentEnabled) {
		validateEventHandlerSequence(lookup.getPickedActions(),
										"[onPicked] event handler for lookup with binding " + 
											lookup.getBinding());
	}

	@Override
	public void visitedOnPickedEventHandler(LookupDescription lookup,
												boolean parentVisible,
												boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitOnClearedEventHandler(LookupDescription lookup,
											boolean parentVisible,
											boolean parentEnabled) {
		validateEventHandlerSequence(lookup.getClearedActions(),
										"[onCleared] event handler for lookup with binding " + 
											lookup.getBinding());
	}

	@Override
	public void visitedOnClearedEventHandler(LookupDescription lookup,
												boolean parentVisible,
												boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitRerenderEventAction(RerenderEventAction rerender,
											EventSource source,
											boolean parentVisible,
											boolean parentEnabled) {
		// no properties to check
	}

	@Override
	public void visitServerSideActionEventAction(ServerSideActionEventAction server,
													boolean parentVisible,
													boolean parentEnabled) {
		validateActionName(server.getActionName(), "[server] event action in an event handler");
	}

	@Override
	public void visitSetDisabledEventAction(SetDisabledEventAction setDisabled,
												boolean parentVisible,
												boolean parentEnabled) {
		String widgetIdentifier = "[setDisabled] event action in an event handler";
		validateBinding(null,
							setDisabled.getBinding(),
							true,
							false,
							false,
							false,
							widgetIdentifier,
							AttributeType.bool);
		String disabledConditionName = setDisabled.getDisabledConditionName();
		if (disabledConditionName == null) {
			throw new MetaDataException(widgetIdentifier + " in " + viewIdentifier + 
											" requires a [disabled] or [enabled] condition name.");
		}
		validateConditionName(disabledConditionName, widgetIdentifier);
	}

	@Override
	public void visitToggleDisabledEventAction(ToggleDisabledEventAction toggleDisabled,
												boolean parentVisible,
												boolean parentEnabled) {
		String widgetIdentifier = "[toggleDisabled] event action in an event handler";
		validateBinding(null,
							toggleDisabled.getBinding(),
							true,
							false,
							false,
							false,
							widgetIdentifier,
							AttributeType.bool);
	}

	@Override
	public void visitToggleVisibilityEventAction(ToggleVisibilityEventAction toggleVisibility,
													boolean parentVisible,
													boolean parentEnabled) {
		String widgetIdentifier = "[toggleVisibility] event action in an event handler";
		validateBinding(null,
							toggleVisibility.getBinding(),
							true,
							false,
							false,
							false,
							widgetIdentifier,
							AttributeType.bool);
	}

	@Override
	public void visitSetInvisibleEventAction(SetInvisibleEventAction setInvisible,
												boolean parentVisible,
												boolean parentEnabled) {
		String widgetIdentifier = "[setInvisible] event action in an event handler";
		validateBinding(null,
							setInvisible.getBinding(),
							true,
							false,
							false,
							false,
							widgetIdentifier,
							AttributeType.bool);
		String invisibleConditionName = setInvisible.getInvisibleConditionName();
		if (invisibleConditionName == null) {
			throw new MetaDataException(widgetIdentifier + " in " + viewIdentifier + 
											" requires an [invisible] or [visible] condition name.");
		}
		validateConditionName(invisibleConditionName, widgetIdentifier);
	}

	@Override
	public void visitSidebar(Sidebar sidebar, boolean parentVisible, boolean parentEnabled) {
		String sidebarIdentifier = "The sidebar";
		if ((sidebar.getPercentageWidth() == null) &&
				(sidebar.getPixelWidth() == null) &&
				(sidebar.getResponsiveWidth() == null)) {
			throw new MetaDataException(sidebarIdentifier + " in " + viewIdentifier + " requires at least one of [percentageWidth, pixelWidth, responsiveWidth]");
		}
		validateConditionName(sidebar.getInvisibleConditionName(), sidebarIdentifier);
	}

	@Override
	public void visitedSidebar(Sidebar sidebar, boolean parentVisible, boolean parentEnabled) {
		// nothing to see here
	}
}
