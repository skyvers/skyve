package modules.kitchensink.FluentDefaultWidgetTest.actions;

import org.skyve.CORE;
import org.skyve.impl.generate.ViewGenerator;
import org.skyve.impl.metadata.repository.DefaultRepository;
import org.skyve.impl.metadata.repository.LockableDynamicRepository;
import org.skyve.impl.metadata.repository.view.Actions;
import org.skyve.impl.metadata.repository.view.ViewMetaData;
import org.skyve.impl.metadata.view.ActionImpl;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.metadata.view.widget.bound.input.KeyboardType;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.metadata.model.Attribute.Sensitivity;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.fluent.FluentBoolean;
import org.skyve.metadata.model.document.fluent.FluentColour;
import org.skyve.metadata.model.document.fluent.FluentContent;
import org.skyve.metadata.model.document.fluent.FluentDate;
import org.skyve.metadata.model.document.fluent.FluentDateTime;
import org.skyve.metadata.model.document.fluent.FluentDecimal10;
import org.skyve.metadata.model.document.fluent.FluentDocument;
import org.skyve.metadata.model.document.fluent.FluentDynamic;
import org.skyve.metadata.model.document.fluent.FluentEnumeratedValue;
import org.skyve.metadata.model.document.fluent.FluentEnumeration;
import org.skyve.metadata.model.document.fluent.FluentGeometry;
import org.skyve.metadata.model.document.fluent.FluentImage;
import org.skyve.metadata.model.document.fluent.FluentInteger;
import org.skyve.metadata.model.document.fluent.FluentMarkup;
import org.skyve.metadata.model.document.fluent.FluentPersistent;
import org.skyve.metadata.model.document.fluent.FluentText;
import org.skyve.metadata.model.document.fluent.FluentTime;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.fluent.FluentDocumentPrivilege;
import org.skyve.metadata.module.fluent.FluentModule;
import org.skyve.metadata.module.fluent.FluentModuleDocument;
import org.skyve.metadata.module.fluent.FluentModuleRoleDocumentAggregateAccess;
import org.skyve.metadata.module.fluent.FluentModuleRoleSingularAccess;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.metadata.user.DocumentPermission;
import org.skyve.metadata.view.Action;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.metadata.view.fluent.FluentCheckBox;
import org.skyve.metadata.view.fluent.FluentColourPicker;
import org.skyve.metadata.view.fluent.FluentCombo;
import org.skyve.metadata.view.fluent.FluentContentImage;
import org.skyve.metadata.view.fluent.FluentContentLink;
import org.skyve.metadata.view.fluent.FluentPassword;
import org.skyve.metadata.view.fluent.FluentRadio;
import org.skyve.metadata.view.fluent.FluentRichText;
import org.skyve.metadata.view.fluent.FluentSlider;
import org.skyve.metadata.view.fluent.FluentSpinner;
import org.skyve.metadata.view.fluent.FluentTextArea;
import org.skyve.metadata.view.fluent.FluentTextField;
import org.skyve.web.WebContext;

import modules.kitchensink.KitchenSinkRepositorySkyveObserver;
import modules.kitchensink.domain.FluentDefaultWidgetTest;

public class GenerateXML implements ServerSideAction<FluentDefaultWidgetTest> {

	private static final String TEST = "Test";
	private static final String UNDERSCORE = "_";
	private static final String DEVELOPER_ROLE_NAME = "dev";

	/**
	 * Creates a FluentDocument with the specified parameters
	 * 
	 * @param name The document name
	 * @param singularAlias The singular alias
	 * @param pluralAlias The plural alias
	 * @param isDynamic Whether to add dynamic property
	 * @param documentation Documentation text
	 * @return FluentDocument instance
	 */
	private static FluentDocument createFluentDocument(String name, String singularAlias, String pluralAlias,
			boolean isDynamic, String documentation) {
		FluentDocument document = new FluentDocument().name(name)
				.singularAlias(singularAlias)
				.pluralAlias(pluralAlias)
				.bizKeyExpression(name);

		// Add dynamic if required
		if (isDynamic) {
			document.dynamic(new FluentDynamic());
		}

		// Add documentation if provided
		if (documentation != null && !documentation.isEmpty()) {
			document.documentation(documentation);
		}

		// Add persistence
		document.persistent(new FluentPersistent().name("TES" + UNDERSCORE + name));

		// Add boolean attributes
		FluentBoolean b = new FluentBoolean().name("bool")
				.documentation("Boolean attribute")
				.displayName("Boolean")
				.sensitivity(Sensitivity.none)
				.required(false)
				.deprecated(false)
				.defaultWidget(new FluentCheckBox());
		document.addBoolean(b);

		FluentBoolean b2 = new FluentBoolean().name("bool2")
				.documentation("Boolean attribute")
				.displayName("Boolean with false tristate")
				.sensitivity(Sensitivity.none)
				.required(false)
				.deprecated(false)
				.defaultWidget(new FluentCheckBox().triState(false));
		document.addBoolean(b2);

		// Add text attributes
		document.addText(new FluentText()
				.name("text")
				.displayName("Text Field")
				.documentation("Text attribute with default text field")
				.length(100)
				.defaultWidget(new FluentTextField()));

		document.addText(new FluentText()
				.name("textArea")
				.displayName("Text Area")
				.documentation("Text attribute with text area widget")
				.length(100)
				.defaultWidget(new FluentTextArea()));

		document.addText(new FluentText()
				.name("richText")
				.displayName("Rich Text")
				.documentation("Text attribute with rich text editor")
				.length(100)
				.defaultWidget(new FluentRichText()));

		// Add integer attribute
		document.addInteger(new FluentInteger()
				.name("integer")
				.displayName("Integer")
				.documentation("Integer attribute")
				.defaultWidget(new FluentTextField().keyboardType(KeyboardType.numeric)));

		// Add decimal attribute
		document.addDecimal10(new FluentDecimal10()
				.name("decimal")
				.displayName("Decimal")
				.documentation("Decimal attribute")
				.defaultWidget(new FluentSpinner()));

		// Add date attribute
		document.addDate(new FluentDate()
				.name("date")
				.displayName("Date")
				.documentation("Date attribute")
				.defaultWidget(new FluentTextField()));

		// Add dateTime attribute
		document.addDateTime(new FluentDateTime()
				.name("dateTime")
				.displayName("Date Time")
				.documentation("DateTime attribute")
				.defaultWidget(new FluentTextField()));

		// Add time attribute
		document.addTime(new FluentTime()
				.name("time")
				.displayName("Time")
				.documentation("Time attribute")
				.defaultWidget(new FluentTextField()));

		// Add enum attribute
		document.addEnumeration(new FluentEnumeration()
				.name("enum")
				.displayName("Enumeration")
				.documentation("Enum attribute")
				.addValue(new FluentEnumeratedValue().code("VALUE1").description("Value 1"))
				.addValue(new FluentEnumeratedValue().code("VALUE2").description("Value 2"))
				.addValue(new FluentEnumeratedValue().code("VALUE3").description("Value 3"))
				.defaultWidget(new FluentCombo()));

		// Add radio buttons for enum
		document.addEnumeration(new FluentEnumeration()
				.name("enumRadio")
				.displayName("Enumeration with Radio Buttons")
				.documentation("Enum attribute with radio buttons")
				.addValue(new FluentEnumeratedValue().code("OPTION1").description("Option 1"))
				.addValue(new FluentEnumeratedValue().code("OPTION2").description("Option 2"))
				.defaultWidget(new FluentRadio()));

		// Add geometry attribute for maps
		document.addGeometry(new FluentGeometry()
				.name("geometry")
				.displayName("Geometry")
				.documentation("Geometry attribute for map")
				.defaultWidget(new org.skyve.metadata.view.fluent.FluentGeometry()));

		// Add color attribute
		document.addColour(new FluentColour()
				.name("color")
				.displayName("Color")
				.documentation("Text attribute for color picker")
				.defaultWidget(new FluentColourPicker()));

		// Add slider for integer
		document.addInteger(new FluentInteger()
				.name("slider")
				.displayName("Slider")
				.documentation("Integer attribute with slider")
				.defaultWidget(new FluentSlider()));

		// Add password field
		document.addText(new FluentText()
				.name("password")
				.displayName("Password")
				.documentation("Password field")
				.length(100)
				.defaultWidget(new FluentPassword()));

		// Add file upload
		document.addContent(new FluentContent()
				.name("file")
				.displayName("File Upload")
				.documentation("File upload field")
				.defaultWidget(new FluentContentLink()));
		
		// Add Image
		document.addImage(new FluentImage()
				.name("picture")
				.displayName("Picture")
				.documentation("Picture")
				.defaultWidget(new FluentContentImage()));

		// Add markup attribute
		document.addMarkup(new FluentMarkup()
				.name("mark")
				.displayName("Markup")
				.documentation("Markup attribute")
				.defaultWidget(new FluentRichText()));

		return document;
	}

	@Override
	public ServerSideActionResult<FluentDefaultWidgetTest> execute(FluentDefaultWidgetTest bean,
			WebContext webContext) throws Exception {

		// Show preview
		bean.setPreviewDocument(Boolean.TRUE);

		// Establish fluent document
		FluentDocument document = createFluentDocument(TEST, TEST, "Tests", false, "Test documentation");

		// Marshall to XML & save
		String xml = XMLMetaData.marshalDocument(document.get(), false);
		bean.setGeneratedXml(xml);

		// load document preview
		loadDocument(bean);

		return new ServerSideActionResult<>(bean);
	}

	/**
	 * Loads dynamic document declaration for Preview Document action.
	 * 
	 * @param Source Manager bean containing all tables in DB
	 * @throws Exception
	 */
	private static void loadDocument(FluentDefaultWidgetTest bean) throws Exception {

		// Fetch repository
		LockableDynamicRepository repository = KitchenSinkRepositorySkyveObserver.KITCHEN_SINK_REPOSITORY;
		DefaultRepository r = (DefaultRepository) CORE.getRepository();
		r.addDelegate(0, repository);

		// Fetch module
		Module m = CORE.getCustomer().getModule(FluentDefaultWidgetTest.MODULE_NAME);

		// Create fluent module from module
		FluentModule fm = new FluentModule().from(m);

		// Establish document title
		String singularAlias = TEST;
		String pluralAlias = TEST;
		bean.setSkyveDocumentName(TEST);
		String documentName = bean.getSkyveDocumentName();

		// Create fluent document
		FluentDocument document = createFluentDocument(documentName, singularAlias, pluralAlias, true, null);

		// Save document
		Document d = repository.putDocument(m, document.get());
		if (fm.findDocument(documentName) == null) {

			fm.addDocument(new FluentModuleDocument().ref(documentName));
			DocumentPermission permissions = DocumentPermission.CRU_C;
			fm.findRole(DEVELOPER_ROLE_NAME)
					.addPrivilege(new FluentDocumentPrivilege().documentName(documentName).permission(permissions))
					.addSingularAccess(new FluentModuleRoleSingularAccess().documentName(documentName))
					.addDocumentAggregateAccess(
							new FluentModuleRoleDocumentAggregateAccess().documentName(documentName));

			// Save module
			m = repository.putModule(fm.get());
		}

		// Set default view
		ViewImpl view = new ViewGenerator(repository).generate(CORE.getCustomer(), d, ViewType.edit.toString());
		ViewMetaData repositoryView = new ViewMetaData();
		repositoryView.setName(ViewType.edit.toString());
		repositoryView.setTitle(view.getTitle());
		repositoryView.getContained().addAll(view.getContained());
		Actions actions = new Actions();
		for (Action action : view.getActions()) {
			actions.getActions().add(((ActionImpl) action).toRepositoryAction());
		}
		repositoryView.setActions(actions);
		repository.putView(CORE.getCustomer(), d, repositoryView);

		// Reset permissions
		((ProvidedRepository) CORE.getRepository()).resetUserPermissions(CORE.getUser());
	}
}
