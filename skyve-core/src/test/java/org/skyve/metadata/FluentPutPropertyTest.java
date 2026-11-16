package org.skyve.metadata;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Supplier;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;
import org.skyve.impl.metadata.model.document.ConditionImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.model.document.UniqueConstraintImpl;
import org.skyve.impl.metadata.model.document.field.Text;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.module.menu.CalendarItem;
import org.skyve.impl.metadata.module.menu.EditItem;
import org.skyve.impl.metadata.module.menu.LinkItem;
import org.skyve.impl.metadata.module.menu.ListItem;
import org.skyve.impl.metadata.module.menu.MapItem;
import org.skyve.impl.metadata.module.menu.MenuGroupImpl;
import org.skyve.impl.metadata.module.menu.MenuImpl;
import org.skyve.impl.metadata.module.menu.TreeItem;
import org.skyve.impl.metadata.module.query.BizQLDefinitionImpl;
import org.skyve.impl.metadata.module.query.MetaDataQueryContentColumnImpl;
import org.skyve.impl.metadata.module.query.MetaDataQueryDefinitionImpl;
import org.skyve.impl.metadata.module.query.MetaDataQueryProjectedColumnImpl;
import org.skyve.impl.metadata.module.query.SQLDefinitionImpl;
import org.skyve.impl.metadata.repository.document.UniqueConstraint;
import org.skyve.impl.metadata.repository.module.ActionPrivilegeMetaData;
import org.skyve.impl.metadata.repository.module.ContentPermission;
import org.skyve.impl.metadata.repository.module.ContentRestriction;
import org.skyve.impl.metadata.repository.module.DocumentPrivilegeMetaData;
import org.skyve.impl.metadata.user.ActionPrivilege;
import org.skyve.impl.metadata.user.DocumentPrivilege;
import org.skyve.impl.metadata.user.RoleImpl;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.metadata.view.container.form.FormItem;
import org.skyve.impl.metadata.view.widget.Button;
import org.skyve.impl.metadata.view.widget.Spacer;
import org.skyve.impl.metadata.view.widget.bound.input.Spinner;
import org.skyve.metadata.behaviour.fluent.FluentAction;
import org.skyve.metadata.behaviour.fluent.FluentIfStatement;
import org.skyve.metadata.behaviour.fluent.FluentSetStatement;
import org.skyve.metadata.customer.fluent.FluentCustomer;
import org.skyve.metadata.customer.fluent.FluentCustomerModules;
import org.skyve.metadata.customer.fluent.FluentCustomerRole;
import org.skyve.metadata.model.document.fluent.FluentAssociation;
import org.skyve.metadata.model.document.fluent.FluentBoolean;
import org.skyve.metadata.model.document.fluent.FluentCollection;
import org.skyve.metadata.model.document.fluent.FluentCollectionUniqueConstraint;
import org.skyve.metadata.model.document.fluent.FluentColour;
import org.skyve.metadata.model.document.fluent.FluentCondition;
import org.skyve.metadata.model.document.fluent.FluentContent;
import org.skyve.metadata.model.document.fluent.FluentDate;
import org.skyve.metadata.model.document.fluent.FluentDateTime;
import org.skyve.metadata.model.document.fluent.FluentDecimal10;
import org.skyve.metadata.model.document.fluent.FluentDecimal2;
import org.skyve.metadata.model.document.fluent.FluentDecimal5;
import org.skyve.metadata.model.document.fluent.FluentDocument;
import org.skyve.metadata.model.document.fluent.FluentDocumentUniqueConstraint;
import org.skyve.metadata.model.document.fluent.FluentEnumeration;
import org.skyve.metadata.model.document.fluent.FluentId;
import org.skyve.metadata.model.document.fluent.FluentImage;
import org.skyve.metadata.model.document.fluent.FluentInteger;
import org.skyve.metadata.model.document.fluent.FluentInverseMany;
import org.skyve.metadata.model.document.fluent.FluentInverseOne;
import org.skyve.metadata.model.document.fluent.FluentLongInteger;
import org.skyve.metadata.model.document.fluent.FluentMarkup;
import org.skyve.metadata.model.document.fluent.FluentMemo;
import org.skyve.metadata.model.document.fluent.FluentText;
import org.skyve.metadata.model.document.fluent.FluentTime;
import org.skyve.metadata.model.document.fluent.FluentTimestamp;
import org.skyve.metadata.module.Module.DocumentRef;
import org.skyve.metadata.module.fluent.FluentBizQL;
import org.skyve.metadata.module.fluent.FluentBizQLReference;
import org.skyve.metadata.module.fluent.FluentCalendarItem;
import org.skyve.metadata.module.fluent.FluentDocumentPrivilege;
import org.skyve.metadata.module.fluent.FluentEditItem;
import org.skyve.metadata.module.fluent.FluentJob;
import org.skyve.metadata.module.fluent.FluentLinkItem;
import org.skyve.metadata.module.fluent.FluentListItem;
import org.skyve.metadata.module.fluent.FluentMapItem;
import org.skyve.metadata.module.fluent.FluentMenu;
import org.skyve.metadata.module.fluent.FluentMenuGroup;
import org.skyve.metadata.module.fluent.FluentMetaDataQuery;
import org.skyve.metadata.module.fluent.FluentMetaDataQueryContentColumn;
import org.skyve.metadata.module.fluent.FluentMetaDataQueryProjectedColumn;
import org.skyve.metadata.module.fluent.FluentMetaDataQueryReference;
import org.skyve.metadata.module.fluent.FluentModule;
import org.skyve.metadata.module.fluent.FluentModuleDocument;
import org.skyve.metadata.module.fluent.FluentModuleRole;
import org.skyve.metadata.module.fluent.FluentSQL;
import org.skyve.metadata.module.fluent.FluentSQLReference;
import org.skyve.metadata.module.fluent.FluentTreeItem;
import org.skyve.metadata.router.fluent.FluentRoute;
import org.skyve.metadata.router.fluent.FluentRouter;
import org.skyve.metadata.router.fluent.FluentUxUi;
import org.skyve.metadata.user.DocumentPermission;
import org.skyve.metadata.view.fluent.FluentAddAction;
import org.skyve.metadata.view.fluent.FluentBizExportAction;
import org.skyve.metadata.view.fluent.FluentBizImportAction;
import org.skyve.metadata.view.fluent.FluentBlurb;
import org.skyve.metadata.view.fluent.FluentButton;
import org.skyve.metadata.view.fluent.FluentCancelAction;
import org.skyve.metadata.view.fluent.FluentChart;
import org.skyve.metadata.view.fluent.FluentCheckBox;
import org.skyve.metadata.view.fluent.FluentCheckMembership;
import org.skyve.metadata.view.fluent.FluentColourPicker;
import org.skyve.metadata.view.fluent.FluentCombo;
import org.skyve.metadata.view.fluent.FluentComparison;
import org.skyve.metadata.view.fluent.FluentComponent;
import org.skyve.metadata.view.fluent.FluentContentSignature;
import org.skyve.metadata.view.fluent.FluentContentUpload;
import org.skyve.metadata.view.fluent.FluentCustomAction;
import org.skyve.metadata.view.fluent.FluentDataGrid;
import org.skyve.metadata.view.fluent.FluentDataGridBoundColumn;
import org.skyve.metadata.view.fluent.FluentDataGridContainerColumn;
import org.skyve.metadata.view.fluent.FluentDataRepeater;
import org.skyve.metadata.view.fluent.FluentDefaultWidget;
import org.skyve.metadata.view.fluent.FluentDefaultsAction;
import org.skyve.metadata.view.fluent.FluentDeleteAction;
import org.skyve.metadata.view.fluent.FluentDialogButton;
import org.skyve.metadata.view.fluent.FluentDownloadAction;
import org.skyve.metadata.view.fluent.FluentDynamicImage;
import org.skyve.metadata.view.fluent.FluentForm;
import org.skyve.metadata.view.fluent.FluentFormColumn;
import org.skyve.metadata.view.fluent.FluentFormItem;
import org.skyve.metadata.view.fluent.FluentFormRow;
import org.skyve.metadata.view.fluent.FluentGeometryMap;
import org.skyve.metadata.view.fluent.FluentHBox;
import org.skyve.metadata.view.fluent.FluentHTML;
import org.skyve.metadata.view.fluent.FluentLabel;
import org.skyve.metadata.view.fluent.FluentLink;
import org.skyve.metadata.view.fluent.FluentListGrid;
import org.skyve.metadata.view.fluent.FluentListMembership;
import org.skyve.metadata.view.fluent.FluentListRepeater;
import org.skyve.metadata.view.fluent.FluentLookupDescription;
import org.skyve.metadata.view.fluent.FluentMapDisplay;
import org.skyve.metadata.view.fluent.FluentNewAction;
import org.skyve.metadata.view.fluent.FluentOKAction;
import org.skyve.metadata.view.fluent.FluentPassword;
import org.skyve.metadata.view.fluent.FluentPrintAction;
import org.skyve.metadata.view.fluent.FluentProgressBar;
import org.skyve.metadata.view.fluent.FluentRadio;
import org.skyve.metadata.view.fluent.FluentRemoveAction;
import org.skyve.metadata.view.fluent.FluentReportAction;
import org.skyve.metadata.view.fluent.FluentRerenderEventAction;
import org.skyve.metadata.view.fluent.FluentRichText;
import org.skyve.metadata.view.fluent.FluentSaveAction;
import org.skyve.metadata.view.fluent.FluentServerSideActionEventAction;
import org.skyve.metadata.view.fluent.FluentSetDisabledEventAction;
import org.skyve.metadata.view.fluent.FluentSetInvisibleEventAction;
import org.skyve.metadata.view.fluent.FluentSidebar;
import org.skyve.metadata.view.fluent.FluentSlider;
import org.skyve.metadata.view.fluent.FluentSpacer;
import org.skyve.metadata.view.fluent.FluentSpinner;
import org.skyve.metadata.view.fluent.FluentStaticImage;
import org.skyve.metadata.view.fluent.FluentTab;
import org.skyve.metadata.view.fluent.FluentTabPane;
import org.skyve.metadata.view.fluent.FluentTextArea;
import org.skyve.metadata.view.fluent.FluentTextField;
import org.skyve.metadata.view.fluent.FluentToggleDisabledEventAction;
import org.skyve.metadata.view.fluent.FluentToggleVisibilityEventAction;
import org.skyve.metadata.view.fluent.FluentTreeGrid;
import org.skyve.metadata.view.fluent.FluentUploadAction;
import org.skyve.metadata.view.fluent.FluentVBox;
import org.skyve.metadata.view.fluent.FluentView;
import org.skyve.metadata.view.fluent.FluentZoomIn;
import org.skyve.metadata.view.fluent.FluentZoomOutAction;

/**
 * Verifies that fluent builders keep custom properties in the order they were added,
 * and that {@code from(...)} carries them across in source order.
 */
@SuppressWarnings("static-method")
class FluentPutPropertyTest {
	private static final List<String> UNSORTED_KEYS = List.of("z", "a");

	/**
	 * Sources for the builders whose {@code from(...)} takes metadata that a fresh
	 * instance of their own {@code get()} type cannot stand in for.
	 */
	private static final Map<Class<?>, Supplier<Object>> FROM_SOURCES = new HashMap<>();
	static {
		FROM_SOURCES.put(FluentCondition.class, ConditionImpl::new);
		FROM_SOURCES.put(FluentDocument.class, DocumentImpl::new);
		FROM_SOURCES.put(FluentDocumentUniqueConstraint.class, UniqueConstraintImpl::new);
		FROM_SOURCES.put(FluentBizQL.class, BizQLDefinitionImpl::new);
		FROM_SOURCES.put(FluentCalendarItem.class, CalendarItem::new);
		FROM_SOURCES.put(FluentEditItem.class, EditItem::new);
		FROM_SOURCES.put(FluentLinkItem.class, LinkItem::new);
		FROM_SOURCES.put(FluentListItem.class, ListItem::new);
		FROM_SOURCES.put(FluentMapItem.class, MapItem::new);
		FROM_SOURCES.put(FluentMenu.class, MenuImpl::new);
		FROM_SOURCES.put(FluentMenuGroup.class, MenuGroupImpl::new);
		FROM_SOURCES.put(FluentMetaDataQuery.class, MetaDataQueryDefinitionImpl::new);
		FROM_SOURCES.put(FluentMetaDataQueryContentColumn.class, MetaDataQueryContentColumnImpl::new);
		FROM_SOURCES.put(FluentMetaDataQueryProjectedColumn.class, MetaDataQueryProjectedColumnImpl::new);
		FROM_SOURCES.put(FluentModule.class, FluentPutPropertyTest::moduleWithMenu);
		FROM_SOURCES.put(FluentModuleDocument.class, DocumentRef::new);
		FROM_SOURCES.put(FluentModuleRole.class, RoleImpl::new);
		FROM_SOURCES.put(FluentSQL.class, SQLDefinitionImpl::new);
		FROM_SOURCES.put(FluentTreeItem.class, TreeItem::new);
		FROM_SOURCES.put(FluentFormItem.class, FluentPutPropertyTest::formItemWithWidget);
		FROM_SOURCES.put(FluentView.class, ViewImpl::new);
	}

	/**
	 * Builders whose {@code from(...)} has no properties to copy: a customer carries none,
	 * and a query reference exposes those of its target query.
	 */
	private static final Set<Class<?>> NOTHING_TO_COPY = Set.<Class<?>>of(FluentCustomer.class,
																			FluentCustomerModules.class,
																			FluentBizQLReference.class,
																			FluentMetaDataQueryReference.class,
																			FluentSQLReference.class);

	/**
	 * Covers every concrete fluent builder that wraps metadata with custom properties.
	 *
	 * <p>One test over all builders, not a {@code @TestFactory}: under surefire's
	 * {@code rerunFailingTestsCount} a dynamic test that fails on every run is reported
	 * as a flake beside its passing siblings, so it cannot fail the build.
	 */
	@Test
	void putPropertyKeepsInsertionOrder() {
		assertAll(Arrays.stream(builders())
							.<Executable>map(b -> () -> assertPutPropertyKeepsInsertionOrder(b)));
	}

	/**
	 * Covers {@code from(...)} on every builder above that has properties to copy.
	 */
	@Test
	void fromCopiesPropertiesInSourceOrder() {
		assertAll(Arrays.stream(builders())
							.filter(b -> ! NOTHING_TO_COPY.contains(b.getClass()))
							.<Executable>map(b -> () -> assertFromCopiesPropertiesInSourceOrder(b)));
	}

	private static Object[] builders() {
		return new Object[] {
			new FluentAction(),
			new FluentIfStatement(),
			new FluentSetStatement(),
			new FluentCustomer(),
			new FluentCustomerModules(),
			new FluentCustomerRole(),
			new FluentAssociation(),
			new FluentBoolean(),
			new FluentCollection(),
			new FluentCollectionUniqueConstraint(),
			new FluentColour(),
			new FluentCondition(),
			new FluentContent(),
			new FluentDate(),
			new FluentDateTime(),
			new FluentDecimal10(),
			new FluentDecimal2(),
			new FluentDecimal5(),
			new FluentDocument(),
			new FluentDocumentUniqueConstraint(),
			new FluentEnumeration(),
			new org.skyve.metadata.model.document.fluent.FluentGeometry(),
			new FluentId(),
			new FluentImage(),
			new FluentInteger(),
			new FluentInverseMany(),
			new FluentInverseOne(),
			new FluentLongInteger(),
			new FluentMarkup(),
			new FluentMemo(),
			new FluentText(),
			new FluentTime(),
			new FluentTimestamp(),
			new FluentBizQL(),
			new FluentBizQLReference(),
			new FluentCalendarItem(),
			new FluentDocumentPrivilege(),
			new FluentEditItem(),
			new FluentJob(),
			new FluentLinkItem(),
			new FluentListItem(),
			new FluentMapItem(),
			new FluentMenu(),
			new FluentMenuGroup(),
			new FluentMetaDataQuery(),
			new FluentMetaDataQueryContentColumn(),
			new FluentMetaDataQueryProjectedColumn(),
			new FluentMetaDataQueryReference(),
			new FluentModule(),
			new FluentModuleDocument(),
			new FluentModuleRole(),
			new FluentSQL(),
			new FluentSQLReference(),
			new FluentTreeItem(),
			new FluentRoute(),
			new FluentRouter(),
			new FluentUxUi(),
			new FluentAddAction(),
			new FluentBizExportAction(),
			new FluentBizImportAction(),
			new FluentBlurb(),
			new FluentButton(),
			new FluentCancelAction(),
			new FluentChart(),
			new FluentCheckBox(),
			new FluentCheckMembership(),
			new FluentColourPicker(),
			new FluentCombo(),
			new FluentComparison(),
			new FluentComponent(),
			new FluentContentSignature(),
			new FluentContentUpload(),
			new FluentCustomAction(),
			new FluentDataGrid(),
			new FluentDataGridBoundColumn(),
			new FluentDataGridContainerColumn(),
			new FluentDataRepeater(),
			new FluentDefaultWidget(),
			new FluentDefaultsAction(),
			new FluentDeleteAction(),
			new FluentDialogButton(),
			new FluentDownloadAction(),
			new FluentDynamicImage(),
			new FluentForm(),
			new FluentFormColumn(),
			new FluentFormItem(),
			new FluentFormRow(),
			new org.skyve.metadata.view.fluent.FluentGeometry(),
			new FluentGeometryMap(),
			new FluentHBox(),
			new FluentHTML(),
			new FluentLabel(),
			new FluentLink(),
			new FluentListGrid(),
			new FluentListMembership(),
			new FluentListRepeater(),
			new FluentLookupDescription(),
			new FluentMapDisplay(),
			new FluentNewAction(),
			new FluentOKAction(),
			new FluentPassword(),
			new FluentPrintAction(),
			new FluentProgressBar(),
			new FluentRadio(),
			new FluentRemoveAction(),
			new FluentReportAction(),
			new FluentRerenderEventAction(),
			new FluentRichText(),
			new FluentSaveAction(),
			new FluentServerSideActionEventAction(),
			new FluentSetDisabledEventAction(),
			new FluentSetInvisibleEventAction(),
			new FluentSidebar(),
			new FluentSlider(),
			new FluentSpacer(),
			new FluentSpinner(),
			new FluentStaticImage(),
			new FluentTab(),
			new FluentTabPane(),
			new FluentTextArea(),
			new FluentTextField(),
			new FluentToggleDisabledEventAction(),
			new FluentToggleVisibilityEventAction(),
			new FluentTreeGrid(),
			new FluentUploadAction(),
			new FluentVBox(),
			new FluentView(),
			new FluentZoomIn(),
			new FluentZoomOutAction()
		};
	}

	@Test
	void leafBuilderFromCopiesPropertiesInSourceOrder() {
		Button button = new Button();
		button.getProperties().put("z", "1");
		button.getProperties().put("a", "2");

		FluentButton fluent = new FluentButton().from(button);

		assertEquals(List.of("z", "a"), List.copyOf(fluent.get().getProperties().keySet()));
	}

	@Test
	void baseBuilderFromCopiesPropertiesInSourceOrder() {
		Text text = new Text();
		text.getProperties().put("z", "1");
		text.getProperties().put("a", "2");

		FluentText fluent = new FluentText().from(text);

		assertEquals(List.of("z", "a"), List.copyOf(fluent.get().getProperties().keySet()));
	}

	@Test
	void subclassedBuilderFromCopiesPropertiesToItsOwnMetadata() {
		Spinner spinner = new Spinner();
		spinner.getProperties().put("z", "1");
		spinner.getProperties().put("a", "2");

		FluentSpinner fluent = new FluentSpinner().from(spinner);

		assertEquals(List.of("z", "a"), List.copyOf(fluent.get().getProperties().keySet()));
	}

	@Test
	void moduleRoleFromCopiesPrivilegeProperties() {
		DocumentPrivilege document = new DocumentPrivilege();
		document.setName("Contact");
		document.setPermission(DocumentPermission.CRUDG);
		putUnsorted(document.getProperties());
		ActionPrivilege action = new ActionPrivilege();
		action.setName("Export");
		action.setDocumentName("Contact");
		putUnsorted(action.getProperties());
		ContentPermission permission = new ContentPermission();
		permission.setDocumentName("Contact");
		permission.setAttributeName("image");
		putUnsorted(permission.getProperties());
		ContentRestriction restriction = new ContentRestriction();
		restriction.setDocumentName("Contact");
		restriction.setAttributeName("notes");
		putUnsorted(restriction.getProperties());
		RoleImpl role = new RoleImpl();
		role.getPrivileges().add(document);
		role.getPrivileges().add(action);
		role.getContentPermissions().add(permission);
		role.getContentRestrictions().add(restriction);

		List<DocumentPrivilegeMetaData> privileges = new FluentModuleRole().from(role).get().getPrivileges();

		assertEquals(1, privileges.size());
		assertNestedPrivilegePropertiesUnsorted(privileges.get(0));
	}

	@Test
	void documentPrivilegeFromCopiesNestedProperties() {
		ActionPrivilegeMetaData action = new ActionPrivilegeMetaData();
		action.setActionName("Export");
		putUnsorted(action.getProperties());
		ContentPermission permission = new ContentPermission();
		permission.setAttributeName("image");
		putUnsorted(permission.getProperties());
		ContentRestriction restriction = new ContentRestriction();
		restriction.setAttributeName("notes");
		putUnsorted(restriction.getProperties());
		DocumentPrivilegeMetaData privilege = new DocumentPrivilegeMetaData();
		putUnsorted(privilege.getProperties());
		privilege.getActions().add(action);
		privilege.getContentPermissions().add(permission);
		privilege.getContentRestrictions().add(restriction);

		assertNestedPrivilegePropertiesUnsorted(new FluentDocumentPrivilege().from(privilege).get());
	}

	private static void assertNestedPrivilegePropertiesUnsorted(DocumentPrivilegeMetaData privilege) {
		assertEquals(UNSORTED_KEYS, List.copyOf(privilege.getProperties().keySet()));
		assertEquals(1, privilege.getActions().size());
		assertEquals(UNSORTED_KEYS, List.copyOf(privilege.getActions().get(0).getProperties().keySet()));
		assertEquals(1, privilege.getContentPermissions().size());
		assertEquals(UNSORTED_KEYS, List.copyOf(privilege.getContentPermissions().get(0).getProperties().keySet()));
		assertEquals(1, privilege.getContentRestrictions().size());
		assertEquals(UNSORTED_KEYS, List.copyOf(privilege.getContentRestrictions().get(0).getProperties().keySet()));
	}

	private static void assertPutPropertyKeepsInsertionOrder(Object builder) throws Exception {
		String name = builder.getClass().getName();
		Method putProperty = builder.getClass().getMethod("putProperty", String.class, String.class);
		assertSame(builder, invoke(builder, putProperty, "z", "1"), name);
		invoke(builder, putProperty, "a", "2");
		invoke(builder, putProperty, "z", "3");

		Map<String, String> properties = properties(metaData(builder));
		assertEquals(UNSORTED_KEYS, List.copyOf(properties.keySet()), name);
		assertEquals("3", properties.get("z"), name);
	}

	private static void assertFromCopiesPropertiesInSourceOrder(Object builder) throws Exception {
		Supplier<Object> registered = FROM_SOURCES.get(builder.getClass());
		Object source = (registered != null) ? registered.get() : metaData(builder.getClass().getConstructor().newInstance());
		putUnsorted(properties(source));

		Method from = fromMethod(builder.getClass(), source);
		// Any parameters ahead of the source are names
		Object[] arguments = new Object[from.getParameterCount()];
		Arrays.fill(arguments, "name");
		arguments[arguments.length - 1] = source;
		invoke(builder, from, arguments);

		assertEquals(UNSORTED_KEYS, List.copyOf(properties(metaData(builder)).keySet()), builder.getClass().getName());
	}

	/**
	 * Invokes {@code method} on {@code builder}, naming the builder if the method throws.
	 */
	private static Object invoke(Object builder, Method method, Object... arguments) throws IllegalAccessException {
		try {
			return method.invoke(builder, arguments);
		}
		catch (InvocationTargetException e) {
			throw new AssertionError(builder.getClass().getName() + "." + method.getName() + "(...) threw", e.getCause());
		}
	}

	/**
	 * Finds the public instance {@code from(...)} whose last parameter accepts {@code source}.
	 */
	private static Method fromMethod(Class<?> type, Object source) {
		for (Method method : type.getMethods()) {
			Class<?>[] parameterTypes = method.getParameterTypes();
			if ("from".equals(method.getName()) &&
					(! method.isBridge()) &&
					(! Modifier.isStatic(method.getModifiers())) &&
					(parameterTypes.length > 0) &&
					parameterTypes[parameterTypes.length - 1].isInstance(source)) {
				return method;
			}
		}
		throw new AssertionError(type.getName() + " has no from(...) that accepts " + source.getClass().getName());
	}

	private static Object metaData(Object builder) throws Exception {
		return builder.getClass().getMethod("get").invoke(builder);
	}

	private static Map<String, String> properties(Object metaData) {
		if (metaData instanceof DecoratedMetaData decorated) {
			return decorated.getProperties();
		}
		// The repository unique constraint holds properties without implementing DecoratedMetaData
		return ((UniqueConstraint) metaData).getProperties();
	}

	private static void putUnsorted(Map<String, String> properties) {
		properties.put("z", "1");
		properties.put("a", "2");
	}

	/**
	 * Returns a module holding a menu, as {@code FluentModule.from} copies the menu unconditionally.
	 */
	private static ModuleImpl moduleWithMenu() {
		ModuleImpl result = new ModuleImpl();
		result.setMenu(new MenuImpl());
		return result;
	}

	/**
	 * Returns a form item holding a widget, as {@code FluentFormItem.from} rejects an empty one.
	 */
	private static FormItem formItemWithWidget() {
		FormItem result = new FormItem();
		result.setWidget(new Spacer());
		return result;
	}
}
