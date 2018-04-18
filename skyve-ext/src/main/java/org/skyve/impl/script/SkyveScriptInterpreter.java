package org.skyve.impl.script;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.commonmark.node.BulletList;
import org.commonmark.node.Code;
import org.commonmark.node.Document;
import org.commonmark.node.Emphasis;
import org.commonmark.node.Heading;
import org.commonmark.node.ListItem;
import org.commonmark.node.Node;
import org.commonmark.node.Paragraph;
import org.commonmark.node.Text;
import org.commonmark.parser.Parser;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.model.document.AssociationImpl;
import org.skyve.impl.metadata.model.document.CollectionImpl;
import org.skyve.impl.metadata.model.document.field.Enumeration;
import org.skyve.impl.metadata.model.document.field.Enumeration.EnumeratedValue;
import org.skyve.impl.metadata.model.document.field.Field;
import org.skyve.impl.metadata.repository.document.BizKey;
import org.skyve.impl.metadata.repository.document.DocumentMetaData;
import org.skyve.impl.metadata.repository.document.ParentDocument;
import org.skyve.impl.metadata.repository.module.Action;
import org.skyve.impl.metadata.repository.module.DocumentPrivilege;
import org.skyve.impl.metadata.repository.module.EditItem;
import org.skyve.impl.metadata.repository.module.GrantedTo;
import org.skyve.impl.metadata.repository.module.Menu;
import org.skyve.impl.metadata.repository.module.ModuleDocument;
import org.skyve.impl.metadata.repository.module.ModuleMetaData;
import org.skyve.impl.metadata.repository.module.ModuleRoleMetaData;
import org.skyve.impl.script.SkyveScriptException.ExceptionType;
import org.skyve.impl.util.PluralUtil;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.document.Association;
import org.skyve.metadata.model.document.Association.AssociationType;
import org.skyve.metadata.model.document.Collection.CollectionType;
import org.skyve.metadata.user.DocumentPermission;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.util.Binder;
import org.skyve.util.Util;

public class SkyveScriptInterpreter {

	private List<DocumentMetaData> documents;
	private List<ModuleMetaData> modules;
	private List<SkyveScriptException> errors;
	private String defaultModule;
	private Parser parser;
	private Node document;
	private String script;

	private static DocumentMetaData currentDocument = null;
	private int currentLine = 0;
	private ModuleMetaData currentModule = null;

	private static Map<String, String> parentDocuments = new HashMap<>();

	private static final String ROLE_MAINTAINER = "Maintainer";
	private static final String ROLE_VIEWER = "Viewer";

	private static String DISPLAY_NAME_PATTERN = "['\"]([^'\"]+)['\"]";
	/**
	 * Matches a markdown collection declaration using square brackets instead of backticks
	 */
	private static String COLLECTION_SHORTHAND_PATTERN = ".*\\s\\[\\w+\\]";
	/**
	 * Matches a markdown enum declaration missing the word enum
	 */
	private static String ENUM_SHORTHAND_PATTERN = "^[-|+]\\s\\*?['\"]?\\w+['\"]?\\*?\\s\\(.+\\)";
	/**
	 * Matches the end of a markdown enum declaration for parsisng substition
	 */
	private static String ENUM_ATTRIBUTE_PATTERN = "\\s(\\(.+\\))$";
	/**
	 * Matches a markdown heading declaration missing the space after #
	 */
	private static String HEADING_SHORTHAND_PATTERN = "^(#{1,2})['\"]?\\w+['\"]?$";
	/**
	 * Matches a markdown list declaration missing the space after the - or +
	 */
	private static String LIST_SHORTHAND_PATTERN = "^[-|+]\\*?['\"]?\\w+['\"]?\\*?\\s.*";
	/**
	 * Matches a markdown required declaration missing the terminating asterisk
	 */
	private static String REQUIRED_SHORTHAND_PATTERN = "^[-|+]\\s(\\*{1}['\"]?\\w+['\"]?)\\s.*";

	/**
	 * Creates a new SkyveScriptInterpreter instance with the
	 * specified script ready to be pre-processed and parsed.
	 * 
	 * @param script The initial skyve script String to be processed
	 */
	public SkyveScriptInterpreter(String script) {
		super();
		parser = Parser.builder().build();

		this.documents = new ArrayList<>();
		this.errors = new ArrayList<>();
		this.modules = new ArrayList<>();
		this.script = script;
	}

	/**
	 * Creates a new SkyveScriptInterpreter instance with the
	 * specified script ready to be pre-processed and parsed.
	 * 
	 * @param script The initial skyve script String to be processed
	 * @param defaultModule The default module name to use if no module is specified
	 */
	public SkyveScriptInterpreter(String script, String defaultModule) {
		this(script);

		this.defaultModule = defaultModule;
	}

	public List<DocumentMetaData> getDocuments() {
		return documents;
	}

	public List<SkyveScriptException> getErrors() {
		return errors;
	}

	public List<ModuleMetaData> getModules() {
		return modules;
	}

	public Node parse() {
		if (script != null && script.length() > 0) {
			this.document = parser.parse(script);
			return this.document;
		}
		throw new IllegalArgumentException("Script was not supplied during construction.");
	}

	/**
	 * Searches for any shorthand/convenience Skyve script phrases and
	 * replaces them with correct markdown which conforms to the specification.
	 * Looks to replace:
	 * <ul>
	 * <li>missing spaces for headings, e.g. <code>#Admin</code> instead of <code># Admin</code>
	 * <li>missing spaces for list items, e.g. <code>-address</code> instead of <code>- address</code>
	 * <li>unterminated asterisks for required fields
	 * <li>missing enum declarations
	 * </ul>
	 * 
	 * @return the cleansed markdown ready to be processed
	 */
	public String preProcess() {
		if (script != null && script.length() > 0) {
			String[] lines = script.split("\n");
			StringBuilder newScript = new StringBuilder();

			for (String line : lines) {
				// look for missing heading spaces
				Pattern p = Pattern.compile(HEADING_SHORTHAND_PATTERN);
				Matcher m = p.matcher(line);

				while (m.find()) {
					final String match = m.group(1);
					if (match != null) {
						line = line.replace(match, match + " ");
					}
				}

				// look for missing list item spaces
				p = Pattern.compile(LIST_SHORTHAND_PATTERN);
				m = p.matcher(line);

				if (m.matches()) {
					line = line.replaceFirst("^-", "- ").replaceFirst("^\\+", "+ ");
				}

				// look for missing termimating asterisk
				p = Pattern.compile(REQUIRED_SHORTHAND_PATTERN);
				m = p.matcher(line);

				while (m.find()) {
					final String match = m.group(1);
					if (match != null) {
						line = line.replace(match, match + "*");
					}
				}

				// look for missing enum
				p = Pattern.compile(ENUM_SHORTHAND_PATTERN);
				m = p.matcher(line);

				if (m.matches()) {
					line = line.replace("(", "enum (");
				}

				// look for collection type shorthand
				p = Pattern.compile(COLLECTION_SHORTHAND_PATTERN);
				m = p.matcher(line);
				if (m.matches()) {
					line = line.replace("[", "`").replace("]", "`");
				}

				if (newScript.length() > 0) {
					newScript.append("\n");
				}
				newScript.append(line);
			}

			script = newScript.toString();
			return script;
		}
		throw new IllegalArgumentException("Script was not supplied during construction.");
	}

	public void process() {
		// parse the script if it has not been done already
		if (document == null) {
			parse();
		}

		// walk the document tree
		if (document instanceof Document) {
			Node node = document.getFirstChild();

			process(node);

			// update the parent of any child collections
			processParentDocuments();
		} else {
			throw new IllegalArgumentException(
					"Invalid markdown supplied, please consult the documentation to supply valid Skyve script.");
		}

		// error if multiple modules found, not currently supported
		if (getModules().size() > 1) {
			addCritical("Multiple modules added, please supply a single module.");
		}
	}

	@SuppressWarnings("boxing")
	private void process(Node node) {
		if (isHeading(node)) {
			Heading heading = (Heading) node;
			if (isHeading1(heading)) {
				currentLine++;
				// new module
				currentModule = new ModuleMetaData();

				if (heading.getFirstChild() != null && heading.getFirstChild() instanceof Text) {
					Text text = (Text) heading.getFirstChild();

					String moduleName = null, moduleTitle = null;

					if (isDisplayName(text.getLiteral())) {
						moduleTitle = extractDisplayName(text.getLiteral());
						moduleName = BindUtil.toJavaInstanceIdentifier(moduleTitle).toLowerCase();
					} else {
						moduleName = text.getLiteral().toLowerCase();
						moduleTitle = toTitleCase(moduleName);
					}

					currentModule.setName(moduleName);
					currentModule.setTitle(moduleTitle);
				} else {
					addCritical("Invalid module definition supplied");
				}

				getModules().add(currentModule);
				process(heading.getNext());
			} else if (isHeading2(heading)) {
				currentLine++;
				// new document
				currentDocument = new DocumentMetaData();

				if (heading.getFirstChild() != null && heading.getFirstChild() instanceof Text) {
					Text text = (Text) heading.getFirstChild();

					String documentName = null, singularAlias = null;

					if (isDisplayName(text.getLiteral().trim())) {
						singularAlias = extractDisplayName(text.getLiteral());
						documentName = BindUtil.toJavaTypeIdentifier(singularAlias);
					} else {
						documentName = text.getLiteral().trim();
						singularAlias = toTitleCase(documentName);
					}

					if (documentName != null) {
						currentDocument.setName(documentName);
						currentDocument.setSingularAlias(singularAlias);
						currentDocument.setPluralAlias(PluralUtil.pluralise(singularAlias));

						BizKey bizKey = new BizKey();
						bizKey.setExpression(singularAlias);
						currentDocument.setBizKey(bizKey);

						Code persistentName = null;
						if (text.getNext() != null && text.getNext() instanceof Code) {
							persistentName = (Code) text.getNext();
						}

						// insert persistent name if provided
						if (persistentName != null) {
							Persistent persistent = new Persistent();
							persistent.setName(persistentName.getLiteral());
							currentDocument.setPersistent(persistent);
						} else {
							// set plural alias to singular if non-persistent
							currentDocument.setPluralAlias(singularAlias);
						}
					} else {
						addError(String.format("Unsupported document name declaratation: %s", text.getLiteral()));
					}
				}

				getDocuments().add(currentDocument);

				// add this document to the module
				if (currentModule == null) {
					initialiseDefaultModule();
				}

				ModuleDocument md = new ModuleDocument();
				md.setRef(currentDocument.getName());
				currentModule.getDocuments().add(md);
				appendRole(currentModule, currentDocument);
				appendMenu(currentModule, currentDocument);

				// set the module home document if not set
				if (currentModule.getHomeDocument() == null) {
					currentModule.setHomeDocument(currentDocument.getName());
					currentModule.setHomeRef(currentDocument.getPersistent() != null ? ViewType.list : ViewType.edit);
				}

				process(heading.getNext());
			}
		} else if (isBulletList(node)) {
			process(node.getFirstChild());
		} else if (isListItem(node)) {
			currentLine++;
			// is this a scalar or association, or a collection
			BulletList list = (BulletList) node.getParent();
			if (list.getBulletMarker() != '-' && list.getBulletMarker() != '+') {
				addError(
						String.format("Unknown list item type: \"%s\". Please use either \"-\" or \"+\".", list.getBulletMarker()));
			} else {
				processListItem(node);
			}

			if (node.getNext() != null) {
				process(node.getNext());
			} else {
				process(node.getParent().getNext());
			}
		}
	}

	/**
	 * Adds a new critical error to the list of errors for this script.
	 */
	private void addCritical(String message) {
		Util.LOGGER.warning(message);
		getErrors().add(new SkyveScriptException(ExceptionType.critical, message, currentLine));

	}

	private void addError(String message) {
		Util.LOGGER.warning(message);
		getErrors().add(new SkyveScriptException(ExceptionType.error, message, currentLine));
	}

	private void addWarning(String message) {
		Util.LOGGER.info(message);
		getErrors().add(new SkyveScriptException(ExceptionType.warning, message, currentLine));
	}

	/**
	 * Appends a new list menu item to the module for the specified document.
	 */
	private static void appendMenu(ModuleMetaData module, DocumentMetaData document) {
		Menu menu = module.getMenu();

		if (menu == null) {
			menu = new Menu();
			module.setMenu(menu);
		}

		GrantedTo maintainer = new GrantedTo();
		maintainer.setRoleName(ROLE_MAINTAINER);

		GrantedTo viewer = new GrantedTo();
		viewer.setRoleName(ROLE_VIEWER);

		List<Action> actions = menu.getActions();

		if (document.getPersistent() != null) {
			org.skyve.impl.metadata.repository.module.ListItem action = new org.skyve.impl.metadata.repository.module.ListItem();
			action.setDocumentName(document.getName());
			action.setName(document.getPluralAlias());

			action.getRoles().add(maintainer);
			action.getRoles().add(viewer);

			actions.add(action);
		} else {
			EditItem action = new EditItem();
			action.setDocumentName(document.getName());
			action.setName(document.getPluralAlias());

			action.getRoles().add(maintainer);
			action.getRoles().add(viewer);

			actions.add(action);
		}
	}

	/**
	 * Appends a viewer and manager role permission for the document to the specified module.
	 */
	private static void appendRole(ModuleMetaData module, DocumentMetaData document) {
		ModuleRoleMetaData viewer = null, maintainer = null;
		if (module.getRoles().size() == 0) {
			viewer = new ModuleRoleMetaData();
			viewer.setName(ROLE_VIEWER);
			viewer.setDescription(String.format("Enough privileges to view %s documents.", module.getTitle()));
			module.getRoles().add(viewer);

			maintainer = new ModuleRoleMetaData();
			maintainer.setName(ROLE_MAINTAINER);
			maintainer.setDescription(String.format("Enough privileges to create and edit %s documents.", module.getTitle()));
			module.getRoles().add(maintainer);
		} else {
			for (ModuleRoleMetaData r : module.getRoles()) {
				if (r.getName().equals(ROLE_VIEWER)) {
					viewer = r;
					continue;
				} else if (r.getName().equals(ROLE_MAINTAINER)) {
					maintainer = r;
					continue;
				}
			}
		}

		if (viewer != null) {
			DocumentPrivilege p = new DocumentPrivilege();
			p.setDocumentName(document.getName());
			p.setPermission(document.getPersistent() != null ? DocumentPermission._R__D : DocumentPermission._____);
			viewer.getPrivileges().add(p);
		}

		if (maintainer != null) {
			DocumentPrivilege p = new DocumentPrivilege();
			p.setDocumentName(document.getName());
			p.setPermission(document.getPersistent() != null ? DocumentPermission.CRUDD : DocumentPermission._____);
			maintainer.getPrivileges().add(p);
		}
	}

	/**
	 * Creates a new Association ready to be added to the current Document.
	 */
	private static Association createAssociation(boolean required, String type, String name, String displayName, Node line) {
		// check if this association definition includes the AssociationType
		AssociationType assocType = extractAssociationType(line);

		AssociationImpl association = new AssociationImpl();
		association.setName(name);
		association.setDisplayName(displayName);
		association.setRequired(required);
		association.setType(assocType);
		association.setDocumentName(type);
		return association;
	}

	private void createAttribute(String[] parts, Node line) {
		// first we have to get the attribute name
		if (parts != null && parts.length > 1) {
			String attributeName = parts[0];
			// remove the first element in the array
			ArrayList<String> pList = new ArrayList<>(Arrays.asList(parts));
			pList.remove(0);
			String[] reducedParts = new String[pList.size()];
			pList.toArray(reducedParts);
			createAttribute(attributeName, reducedParts, false, line);
		}
	}

	private void createAttribute(String attributeName, String[] parts, boolean required, Node line) {
		// identify the type from the parts
		String type = null;
		if (parts.length >= 1) {
			type = parts[0];
		}

		if (type != null) {
			String name = null, displayName = null;

			if (isDisplayName(attributeName)) {
				displayName = extractDisplayName(attributeName);
				name = BindUtil.toJavaInstanceIdentifier(displayName);
			} else {
				name = attributeName;
				displayName = toTitleCase(name);
			}

			Field field = null;

			// check if this is a scalar attribute
			switch (type) {
				case "boolean":
					field = createFieldBoolean(required, name, displayName);
					break;
				case "colour":
					field = createFieldColour(required, name, displayName);
					break;
				case "content":
					field = createFieldContent(required, name, displayName);
					break;
				case "date":
					field = createFieldDate(required, name, displayName);
					break;
				case "dateTime":
					field = createFieldDateTime(required, name, displayName);
					break;
				case "decimal10":
					field = createFieldDecimal10(required, name, displayName);
					break;
				case "decimal2":
					field = createFieldDecimal2(required, name, displayName);
					break;
				case "decimal5":
					field = createFieldDecimal5(required, name, displayName);
					break;
				case "geometry":
					field = createFieldGeometry(required, name, displayName);
					break;
				case "id":
					field = createFieldId(required, name, displayName);
					break;
				case "integer":
					field = createFieldInteger(required, name, displayName);
					break;
				case "longInteger":
					field = createFieldLongInteger(required, name, displayName);
					break;
				case "memo":
					field = createFieldMemo(required, name, displayName);
					break;
				case "markup":
					field = createFieldMarkup(required, name, displayName);
					break;
				case "time":
					field = createFieldTime(required, name, displayName);
					break;
				case "timestamp":
					field = createFieldTimestamp(required, name, displayName);
					break;
				case "enum":
					// check we have the values specified in brackets
					if (parts.length == 2) {
						String brackets = parts[1];
						if (brackets.startsWith("(") && brackets.endsWith(")")) {
							Enumeration e = createFieldEnum(required, name, displayName, brackets);
							currentDocument.getAttributes().add(e);
						} else {
							addError(String.format("Enum attribute: %s missing required value array", attributeName));
						}
					} else {
						addWarning(String.format("Unsupported enum definition: %s",
								Arrays.toString(parts).replace(",,", ",").replace("[", "").replace("]", "")));
					}
					break;
				case "text":
					// check we have a length
					if (parts.length == 2) {
						try {
							int length = Integer.parseInt(parts[1]);
							org.skyve.impl.metadata.model.document.field.Text text = createFieldText(required, name, displayName,
									length);
							currentDocument.getAttributes().add(text);
						} catch (NumberFormatException nfe) {
							addError(
									String.format("Text attribute: %s length [%s] was not an integer", attributeName, parts[1]));
						}
					} else {
						addError(String.format("Text attribute: %s missing required field length", attributeName));
					}
					break;
				default:
					// did not match scalar attribute, check if association or collection definition
					if (isAssociationDefinition(line, type, parts)) {
						Association association = createAssociation(required, type, name, displayName, line);
						currentDocument.getAttributes().add(association);
					} else if (isCollectionDefinition(line, type, parts)) {
						CollectionImpl collection = createCollection(required, type, name, displayName, line);
						currentDocument.getAttributes().add(collection);
					} else {
						addWarning(String.format("Unsupported attribute: %s [%s]", attributeName, parts[0]));
					}
					break;
			}

			if (field != null) {
				currentDocument.getAttributes().add(field);
			}
		} else {
			addWarning(String.format("Unsupported attribute definition: %s",
					Arrays.toString(parts).replace(",,", ",").replace("[", "").replace("]", "")));
		}
	}

	/**
	 * Creates a new Collection ready to be added to the current document.
	 */
	@SuppressWarnings("boxing")
	private static CollectionImpl createCollection(boolean required, String type, String name, String displayName, Node line) {
		CollectionType collectionType = extractCollectionType(line);

		// if the collection type is child, store it for processing the parent at the end
		if (CollectionType.child.equals(collectionType)) {
			parentDocuments.put(type, currentDocument.getName());
		}

		CollectionImpl collection = new CollectionImpl();
		collection.setName(name);
		collection.setDisplayName(displayName);
		collection.setMinCardinality(required ? 1 : 0);
		collection.setDocumentName(type);
		collection.setType(collectionType);
		return collection;
	}

	private static Field createFieldBoolean(boolean required, String name, String displayName) {
		Field field = new org.skyve.impl.metadata.model.document.field.Boolean();
		return setCommonFieldAttributes(field, required, name, displayName);
	}

	private static Field createFieldColour(boolean required, String name, String displayName) {
		Field field = new org.skyve.impl.metadata.model.document.field.Colour();
		return setCommonFieldAttributes(field, required, name, displayName);
	}

	private static Field createFieldContent(boolean required, String name, String displayName) {
		Field field = new org.skyve.impl.metadata.model.document.field.Content();
		return setCommonFieldAttributes(field, required, name, displayName);
	}

	private static Field createFieldDate(boolean required, String name, String displayName) {
		Field field = new org.skyve.impl.metadata.model.document.field.Date();
		return setCommonFieldAttributes(field, required, name, displayName);
	}

	private static Field createFieldDateTime(boolean required, String name, String displayName) {
		Field field = new org.skyve.impl.metadata.model.document.field.DateTime();
		return setCommonFieldAttributes(field, required, name, displayName);
	}

	private static Field createFieldDecimal2(boolean required, String name, String displayName) {
		Field field = new org.skyve.impl.metadata.model.document.field.Decimal2();
		return setCommonFieldAttributes(field, required, name, displayName);
	}

	private static Field createFieldDecimal5(boolean required, String name, String displayName) {
		Field field = new org.skyve.impl.metadata.model.document.field.Decimal5();
		return setCommonFieldAttributes(field, required, name, displayName);
	}

	private static Field createFieldDecimal10(boolean required, String name, String displayName) {
		Field field = new org.skyve.impl.metadata.model.document.field.Decimal10();
		return setCommonFieldAttributes(field, required, name, displayName);
	}

	private static Enumeration createFieldEnum(boolean required, String name, String displayName, String brackets) {
		org.skyve.impl.metadata.model.document.field.Enumeration e = new org.skyve.impl.metadata.model.document.field.Enumeration();
		setCommonFieldAttributes(e, required, name, displayName);

		String[] values = brackets.split(",");
		if (values.length > 0) {
			for (String v : values) {
				v = v.trim().replace("(", "").replace(")", "");
				EnumeratedValue ev = new EnumeratedValue();

				// check for quotes
				if (v.startsWith("\"") || v.startsWith("'") || v.indexOf(" ") != -1) {
					v = v.replaceAll("[\"']", "");
					ev.setDescription(v);
					ev.setCode(Binder.toJavaTypeIdentifier(v));
				} else {
					ev.setCode(v);
				}

				e.getValues().add(ev);
			}
		}

		return e;
	}

	private static Field createFieldGeometry(boolean required, String name, String displayName) {
		Field field = new org.skyve.impl.metadata.model.document.field.Geometry();
		return setCommonFieldAttributes(field, required, name, displayName);
	}

	private static Field createFieldId(boolean required, String name, String displayName) {
		Field field = new org.skyve.impl.metadata.model.document.field.Id();
		return setCommonFieldAttributes(field, required, name, displayName);
	}

	private static Field createFieldInteger(boolean required, String name, String displayName) {
		Field field = new org.skyve.impl.metadata.model.document.field.Integer();
		return setCommonFieldAttributes(field, required, name, displayName);
	}

	private static Field createFieldLongInteger(boolean required, String name, String displayName) {
		Field field = new org.skyve.impl.metadata.model.document.field.LongInteger();
		return setCommonFieldAttributes(field, required, name, displayName);
	}

	private static Field createFieldMemo(boolean required, String name, String displayName) {
		Field field = new org.skyve.impl.metadata.model.document.field.Memo();
		return setCommonFieldAttributes(field, required, name, displayName);
	}

	private static Field createFieldMarkup(boolean required, String name, String displayName) {
		Field field = new org.skyve.impl.metadata.model.document.field.Markup();
		return setCommonFieldAttributes(field, required, name, displayName);
	}

	private static org.skyve.impl.metadata.model.document.field.Text createFieldText(boolean required, String name,
			String displayName, int length) {
		org.skyve.impl.metadata.model.document.field.Text text = new org.skyve.impl.metadata.model.document.field.Text();
		setCommonFieldAttributes(text, required, name, displayName);
		text.setLength(length);
		return text;
	}

	private static Field createFieldTime(boolean required, String name, String displayName) {
		Field field = new org.skyve.impl.metadata.model.document.field.Time();
		return setCommonFieldAttributes(field, required, name, displayName);
	}

	private static Field createFieldTimestamp(boolean required, String name, String displayName) {
		Field field = new org.skyve.impl.metadata.model.document.field.Timestamp();
		return setCommonFieldAttributes(field, required, name, displayName);
	}

	/**
	 * Returns the AssociationType for the supplied node, or defaults
	 * to Aggregation if none was found.
	 */
	private static AssociationType extractAssociationType(final Node line) {
		Node node = line;
		while (node.getNext() != null) {
			node = node.getNext();
			if (node instanceof Code) {
				return AssociationType.valueOf(((Code) node).getLiteral());
			}
		}

		return AssociationType.aggregation;
	}

	/**
	 * Returns the CollectionType for the supplied node, or defaults
	 * to Aggregation if none was found.
	 */
	private static CollectionType extractCollectionType(final Node line) {
		Node node = line;
		while (node.getNext() != null) {
			node = node.getNext();
			if (node instanceof Code) {
				return CollectionType.valueOf(((Code) node).getLiteral());
			}
		}

		return CollectionType.aggregation;
	}

	/**
	 * Returns the display name for the supplied text.
	 */
	private static String extractDisplayName(String text) {
		final Pattern p = Pattern.compile(DISPLAY_NAME_PATTERN);
		final Matcher m = p.matcher(text);

		while (m.find()) {
			final String match = m.group(1);
			if (match != null) {
				return match;
			}
		}

		return null;
	}

	/**
	 * Returns the literal text within a Node if the node is not null and of type Text.
	 * 
	 * @param node The node to check for text
	 * @return The text
	 */
	private static String getTextFromNode(Node node) {
		if (node != null && node instanceof Text) {
			Text text = (Text) node;
			return text.getLiteral();
		}

		return null;
	}

	/**
	 * Sets up the current module to add documents to if the module
	 * has not been specified as part of the script.
	 */
	private void initialiseDefaultModule() {
		currentModule = new ModuleMetaData();

		if (defaultModule != null) {
			String moduleName = null, moduleTitle = null;

			moduleName = defaultModule;
			moduleTitle = toTitleCase(moduleName);

			currentModule.setName(moduleName);
			currentModule.setTitle(moduleTitle);

			getModules().add(currentModule);
		} else {
			addCritical("Default module required but not supplied");
		}
	}

	/**
	 * Returns true if the specified type describes an association:
	 * <ul>
	 * <li>the attribute belongs to an unordered list
	 * <li>the first letter is uppercase
	 * <li>there are no following words on the line
	 * </ul>
	 * 
	 * @param type The type to be checked
	 * @param parts The remaining tokens for the line
	 * @return true if an association, false otherwise
	 */
	private static boolean isAssociationDefinition(Node line, String type, String[] parts) {
		if (isChildOfDashMarkerList(line)) {
			// if (isChildOfBulletList(line)) {
			if (type != null && Character.isUpperCase(type.charAt(0)) && parts.length == 1) {
				return true;
			}
		}

		return false;
	}

	/**
	 * Recursively checks the parent of the specified node
	 * to see if it is a descendant of an a bullet list (
	 * <ul>
	 * ) specified with a -.
	 */
	private static boolean isChildOfDashMarkerList(Node node) {
		if (node.getParent() != null) {
			if (node.getParent() instanceof BulletList) {
				BulletList list = (BulletList) node.getParent();
				return list.getBulletMarker() == '-';
			}
			return isChildOfDashMarkerList(node.getParent());
		}
		return false;
	}

	/**
	 * Recursively checks the parent of the specified node
	 * to see if it is a descendant of an a bullet list (
	 * <ul>
	 * ) specified with a +.
	 */
	private static boolean isChildOfPlusMarkerList(Node node) {
		if (node.getParent() != null) {
			if (node.getParent() instanceof BulletList) {
				BulletList list = (BulletList) node.getParent();
				return list.getBulletMarker() == '+';
			}
			return isChildOfPlusMarkerList(node.getParent());
		}
		return false;
	}

	/**
	 * Returns true if the specified node is a {@link BulletList}.
	 */
	private static boolean isBulletList(Node node) {
		if (node instanceof BulletList) {
			return true;
		}
		return false;
	}

	/**
	 * Returns true if the specified type describes a acollection:
	 * <ul>
	 * <li>the attribute belongs to an ordered list
	 * <li>the first letter is uppercase
	 * <li>there are no following words on the line
	 * </ul>
	 * 
	 * @return true if a collection, false otherwise
	 */
	private static boolean isCollectionDefinition(Node line, String type, String[] parts) {
		if (isChildOfPlusMarkerList(line)) {
			if (type != null && Character.isUpperCase(type.charAt(0)) && parts.length == 1) {
				return true;
			}
		}

		return false;
	}

	/**
	 * Returns true if the supplied text matches the expression for a display name
	 * (text surrounded by single or double quotes).
	 */
	private static boolean isDisplayName(String text) {
		final Pattern p = Pattern.compile(DISPLAY_NAME_PATTERN);
		final Matcher m = p.matcher(text);

		return m.matches();
	}

	/**
	 * Returns true if the specified node is a Heading.
	 */
	private static boolean isHeading(Node node) {
		if (node instanceof Heading) {
			return true;
		}
		return false;
	}

	/**
	 * Returns true if the specified node is a Heading with level 1,
	 * indicating a module definition.
	 */
	@SuppressWarnings("unused")
	private static boolean isHeading1(Node node) {
		if (node instanceof Heading) {
			Heading heading = (Heading) node;
			return isHeading1(heading);
		}
		return false;
	}

	/**
	 * Returns true if the specified node is a Heading with level 1,
	 * indicating a module definition.
	 */
	private static boolean isHeading1(Heading heading) {
		return heading.getLevel() == 1 ? true : false;
	}

	/**
	 * Returns true if the specified node is a Heading with level 2,
	 * indicating a document definition.
	 */
	@SuppressWarnings("unused")
	private static boolean isHeading2(Node node) {
		if (node instanceof Heading) {
			Heading heading = (Heading) node;
			return isHeading2(heading);
		}
		return false;
	}

	/**
	 * Returns true if the specified node is a Heading with level 2,
	 * indicating a document definition.
	 */
	private static boolean isHeading2(Heading heading) {
		return heading.getLevel() == 2 ? true : false;
	}

	/**
	 * Returns true if the specified node is a {@link ListItem}.
	 */
	private static boolean isListItem(Node node) {
		if (node instanceof ListItem) {
			return true;
		}
		return false;
	}

	private void parseAttribute(Node node) {
		if (node.getFirstChild() != null && node.getFirstChild() instanceof Emphasis) {
			// required attribute
			Emphasis em = (Emphasis) node.getFirstChild();
			String attributeName = getTextFromNode(em.getFirstChild());

			// get the rest of the attribute spec
			if (em.getNext() != null && em.getNext() instanceof Text) {
				String remainingDefinition = getTextFromNode(em.getNext());
				String[] parts = splitAttribute(remainingDefinition);
				createAttribute(attributeName, parts, true, em);
			} else {
				addWarning("Invalid attribute definition: " + node.getLastChild().toString());
			}

		} else if (node.getFirstChild() != null && node.getFirstChild() instanceof Text) {
			String line = getTextFromNode(node.getFirstChild());
			String[] parts = splitAttribute(line);
			createAttribute(parts, node.getFirstChild());
		}
	}

	private void processListItem(Node item) {
		if (item.getFirstChild() != null
				&& (item.getFirstChild() instanceof Text || item.getFirstChild() instanceof Paragraph)) {
			if (item.getFirstChild() instanceof Paragraph) {
				Node paragraph = item.getFirstChild();
				parseAttribute(paragraph);
			} else {
				parseAttribute(item);
			}
		}
	}

	private void processParentDocuments() {
		for (Map.Entry<String, String> e : parentDocuments.entrySet()) {
			for (DocumentMetaData d : getDocuments()) {
				if (d.getName().equals(e.getKey())) {
					ParentDocument parent = new ParentDocument();
					parent.setParentDocumentName(e.getValue());
					d.setParentDocument(parent);
					break;
				}
			}
		}
	}

	/**
	 * Sets the common field attributes (name, displayName, required) for a base scalar field.
	 */
	private static Field setCommonFieldAttributes(Field field, boolean required, String name, String displayName) {
		field.setName(name);
		field.setRequired(required);
		field.setDisplayName(displayName);
		return field;
	}

	/**
	 * Splits the attribute declaration into individual parts so they can
	 * be processed one at a time, e.g. type, length.
	 * 
	 * @param line The attribute line to be parsed
	 * @return A String array of line parts
	 */
	private static String[] splitAttribute(final String line) {
		String match = null, line2 = line;

		// if this is an enum, treat that separately as a single part
		Pattern p = Pattern.compile(ENUM_ATTRIBUTE_PATTERN);
		Matcher m = p.matcher(line2);

		while (m.find()) {
			match = m.group(1);
			if (match != null) {
				line2 = line2.replace(match, "");
			}
		}

		String[] parts = line2.trim().split("\\s");

		// append the enum definition if one was found
		if (match != null) {
			parts = Arrays.copyOf(parts, parts.length + 1);
			parts[parts.length - 1] = match;
		}

		return parts;
	}

	/**
	 * Fashion a title case identifier from the given string.
	 * 
	 * @param string The string to convert
	 * @return A title case string. First letter of each word upper cased with spaces between words.
	 */
	private static String toTitleCase(String string) {
		String javaIdentifierName = BindUtil.toJavaTypeIdentifier(string);
		StringBuilder sb = new StringBuilder(javaIdentifierName.length() + 5);

		for (int i = 0, l = javaIdentifierName.length(); i < l; i++) {
			char ch = javaIdentifierName.charAt(i);
			if (Character.isUpperCase(ch)) {
				boolean nextCharLowerCase = false;
				boolean prevCharLowerCase = false;
				int nextIndex = i + 1;
				int prevIndex = i - 1;
				if (nextIndex < l) {
					char nextChar = javaIdentifierName.charAt(nextIndex);
					nextCharLowerCase = Character.isLowerCase(nextChar);
				}
				if (prevIndex >= 0) {
					char prevChar = javaIdentifierName.charAt(prevIndex);
					prevCharLowerCase = Character.isLowerCase(prevChar);
				}

				// if the previous char was upper case then don't add a space
				if ((prevCharLowerCase || nextCharLowerCase) && (i > 0)) {
					sb.append(' ');
				}
				sb.append(ch);
			} else {
				sb.append(ch);
			}
		}

		return sb.toString();
	}
}
