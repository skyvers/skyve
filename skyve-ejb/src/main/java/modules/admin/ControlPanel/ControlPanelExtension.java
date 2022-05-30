package modules.admin.ControlPanel;

import java.io.File;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Map;
import java.util.TreeMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.UtilImpl.MapType;
import org.skyve.util.Binder;
import org.skyve.util.PushMessage;
import org.skyve.util.Util;

import modules.admin.domain.ControlPanel;
import modules.admin.domain.Generic;
import modules.admin.domain.ModuleDocument;

public class ControlPanelExtension extends ControlPanel {

	private static final String ENVIRONMENT_SHOW_SETUP_KEY = "showSetup";
	private static final String ENVIRONMENT_SUPPORT_EMAIL_ADDRESS_KEY = "supportEmailAddress";
	private static final String ENVIRONMENT_MODULE_DIRECTORY_KEY = "moduleDirectory";
	private static final String ENVIRONMENT_CUSTOMER_KEY = "customer";
	private static final String ENVIRONMENT_IDENTIFIER_KEY = "identifier";
	private static final String SMTP_TEST_BOGUS_SEND_KEY = "testBogusSend";
	private static final String SMTP_TEST_RECIPIENT_KEY = "testRecipient";
	private static final String SMTP_SENDER_KEY = "sender";
	private static final String SMTP_PWD_KEY = "pwd";
	private static final String SMTP_UID_KEY = "uid";
	private static final String SMTP_PORT_KEY = "port";
	private static final String SMTP_SERVER_KEY = "server";
	private static final String MAP_TYPE_KEY = "type";
	private static final String QUOTE = "\"";
	private static final String NULL_STRING = "null";
	public static final String DISPLAY_DELIM = ".";
	public static final String ENVIRONMENT_STANZA_KEY = "environment";
	public static final String SMTP_STANZA_KEY = "smtp";
	public static final String MAP_STANZA_KEY = "map";
	public static final String API_STANZA_KEY = "api";
	public static final String CK_EDITOR_CONFIG_FILE_URL = "ckEditorConfigFileUrl";
	public static final String GOOGLE_RECAPTCHA_SITE_KEY = "googleRecaptchaSiteKey";
	public static final String GOOGLE_MAPS_V3_KEY = "googleMapsV3Key";

	private static final long serialVersionUID = -6204655500999983605L;

	private String unescapedResults;

	/**
	 * Create a new DocumentName to hold the module and document name for creating test data.
	 * 
	 * @param documentName The document name to add
	 * @return A {@link ModuleDocument} containing the module and document name for processing later
	 */
	public ModuleDocument addDocumentToCreate(final String documentName) {
		ModuleDocument docName = ModuleDocument.newInstance();
		docName.setModuleName(getTestModuleName());
		docName.setDocumentName(documentName);
		return docName;
	}
	
	public void trapException(Exception e) {
		StringWriter sw = new StringWriter(512);
		PrintWriter pw = new PrintWriter(sw);
		e.printStackTrace(pw);
		setResults(sw.toString());
	}

	/**
	 * Overriden to escape {, < & >.
	 * Add a new line out the front to line up all lines to the left of the blurb.
	 */
	@Override
	public void setResults(String results) {
		setResults(results, true);
	}
	
	public void setResults(String results, boolean escape) {
		unescapedResults = results;
		if ((escape) && (results != null)) {
			super.setResults('\n' + results.replace("{", "\\{").replace("<", "&lt;").replace(">", "&gt;"));
		}
		else {
			super.setResults(results);
		}
	}

	public String getUnescapedResults() {
		return unescapedResults;
	}

	@Override
	public Boolean getBizletTrace() {
		return Boolean.valueOf(UtilImpl.BIZLET_TRACE);
	}

	@Override
	public void setBizletTrace(Boolean bizletTrace) {
		UtilImpl.BIZLET_TRACE = Boolean.TRUE.equals(bizletTrace);
	}

	@Override
	public Boolean getCommandTrace() {
		return Boolean.valueOf(UtilImpl.COMMAND_TRACE);
	}

	@Override
	public void setCommandTrace(Boolean commandTrace) {
		UtilImpl.COMMAND_TRACE = Boolean.TRUE.equals(commandTrace);
	}

	@Override
	public Boolean getContentTrace() {
		return Boolean.valueOf(UtilImpl.CONTENT_TRACE);
	}

	@Override
	public void setContentTrace(Boolean contentTrace) {
		UtilImpl.CONTENT_TRACE = Boolean.TRUE.equals(contentTrace);
	}

	@Override
	public Boolean getDirtyTrace() {
		return Boolean.valueOf(UtilImpl.DIRTY_TRACE);
	}

	@Override
	public void setDirtyTrace(Boolean dirtyTrace) {
		UtilImpl.DIRTY_TRACE = Boolean.TRUE.equals(dirtyTrace);
	}

	@Override
	public Boolean getFacesTrace() {
		return Boolean.valueOf(UtilImpl.FACES_TRACE);
	}

	@Override
	public void setFacesTrace(Boolean facesTrace) {
		UtilImpl.FACES_TRACE = Boolean.TRUE.equals(facesTrace);
	}

	@Override
	public Boolean getHttpTrace() {
		return Boolean.valueOf(UtilImpl.HTTP_TRACE);
	}

	@Override
	public void setHttpTrace(Boolean httpTrace) {
		UtilImpl.HTTP_TRACE = Boolean.TRUE.equals(httpTrace);
	}

	/*
	 * Cant influence this setting as it is set in the hibernate session factory
	 * 
	 * @Override
	 * public Boolean getPrettySqlOutput() {
	 * return Boolean.valueOf(UtilImpl.PRETTY_SQL_OUTPUT);
	 * }
	 * 
	 * @Override
	 * public void setPrettySqlOutput(Boolean prettySqlOutput) {
	 * UtilImpl.PRETTY_SQL_OUTPUT = Boolean.TRUE.equals(prettySqlOutput);
	 * }
	 */
	@Override
	public Boolean getQueryTrace() {
		return Boolean.valueOf(UtilImpl.QUERY_TRACE);
	}

	@Override
	public void setQueryTrace(Boolean queryTrace) {
		UtilImpl.QUERY_TRACE = Boolean.TRUE.equals(queryTrace);
	}

	@Override
	public Boolean getSecurityTrace() {
		return Boolean.valueOf(UtilImpl.SECURITY_TRACE);
	}

	@Override
	public void setSecurityTrace(Boolean securityTrace) {
		UtilImpl.SECURITY_TRACE = Boolean.TRUE.equals(securityTrace);
	}

	/*
	 * Cant influence this setting as it is set in the hibernate session factory
	 * 
	 * @Override
	 * public Boolean getSqlTrace() {
	 * return Boolean.valueOf(UtilImpl.SQL_TRACE);
	 * }
	 * 
	 * @Override
	 * public void setSqlTrace(Boolean sqlTrace) {
	 * UtilImpl.SQL_TRACE = Boolean.TRUE.equals(sqlTrace);
	 * }
	 */
	@Override
	public Boolean getXmlTrace() {
		return Boolean.valueOf(UtilImpl.XML_TRACE);
	}

	@Override
	public void setXmlTrace(Boolean xmlTrace) {
		UtilImpl.XML_TRACE = Boolean.TRUE.equals(xmlTrace);
	}

	/**
	 * Load startup configuration ready for modification
	 * 
	 * @throws Exception
	 */
	public void loadStartupConfiguration() throws Exception {

		getStartupProperties().clear();
		addProperty(SMTP_STANZA_KEY + DISPLAY_DELIM + SMTP_SERVER_KEY, UtilImpl.SMTP, "Mail Server URL");
		addProperty(SMTP_STANZA_KEY + DISPLAY_DELIM + SMTP_PORT_KEY, String.valueOf(UtilImpl.SMTP_PORT), "Mail Server Port");
		addProperty(SMTP_STANZA_KEY + DISPLAY_DELIM + SMTP_UID_KEY, UtilImpl.SMTP_UID, "Mail Server User Name");
		addProperty(SMTP_STANZA_KEY + DISPLAY_DELIM + SMTP_PWD_KEY, UtilImpl.SMTP_PWD, "Mail server password");
		addProperty(SMTP_STANZA_KEY + DISPLAY_DELIM + SMTP_SENDER_KEY, UtilImpl.SMTP_SENDER, "Sender header value");
		addProperty(SMTP_STANZA_KEY + DISPLAY_DELIM + SMTP_TEST_RECIPIENT_KEY, UtilImpl.SMTP_TEST_RECIPIENT, "Test Recipient (all emails only sent to this email address)");
		addProperty(SMTP_STANZA_KEY + DISPLAY_DELIM + SMTP_TEST_BOGUS_SEND_KEY, Boolean.toString(UtilImpl.SMTP_TEST_BOGUS_SEND),
				"Test Bogus Send - Set true so emails are never sent, they are logged)");
		// TODO - handle extended SMTP properties

		addProperty(MAP_STANZA_KEY + DISPLAY_DELIM + MAP_TYPE_KEY, UtilImpl.MAP_TYPE.toString(), "Map API to use - gmap or leaflet");

		addProperty(API_STANZA_KEY + DISPLAY_DELIM + GOOGLE_MAPS_V3_KEY, UtilImpl.GOOGLE_MAPS_V3_API_KEY,
				"Google Maps API Key - to obtain a Google maps API Key go to https://developers.google.com/maps/documentation/javascript/get-api-key");
		addProperty(API_STANZA_KEY + DISPLAY_DELIM + GOOGLE_RECAPTCHA_SITE_KEY, UtilImpl.GOOGLE_RECAPTCHA_SITE_KEY, "Google Recaptcha Site Key");
		addProperty(API_STANZA_KEY + DISPLAY_DELIM + CK_EDITOR_CONFIG_FILE_URL, UtilImpl.CKEDITOR_CONFIG_FILE_URL, "CKEditor Config File URL");

		@SuppressWarnings("unchecked")
		Map<String, Object> api = (Map<String, Object>) UtilImpl.CONFIGURATION.get(API_STANZA_KEY);
		if (api != null) {
			for (String k : api.keySet()) {
				// ignore keys already loaded above
				if (!k.equals(GOOGLE_MAPS_V3_KEY) && !k.equals(GOOGLE_RECAPTCHA_SITE_KEY) && !k.equals(CK_EDITOR_CONFIG_FILE_URL)) {

					// load this new key value - may be a string, or a hashmap
					Object value = api.get(k);
					if (value instanceof String) {
						addProperty(API_STANZA_KEY + DISPLAY_DELIM + "" + k, (String) value, "");
					} else {
						// multiple depth level stanzas not yet supported
						setAddKeyNotSupported(Boolean.FALSE);

						@SuppressWarnings("unchecked")
						Map<String, String> keyValue = (Map<String, String>) api.get(k);
						for (String innerK : keyValue.keySet()) {
							Object innerV = keyValue.get(innerK);
							if (innerV instanceof String) {
								addProperty(API_STANZA_KEY + DISPLAY_DELIM + "" + k + DISPLAY_DELIM + innerK, (String) innerV, "");
							} else {
								throw new ValidationException(new Message("Configuration includes unsupported types - edit the json settings file directly"));
							}
						}
					}
				}
			}
		}

		addProperty(ENVIRONMENT_STANZA_KEY + DISPLAY_DELIM + ENVIRONMENT_IDENTIFIER_KEY, UtilImpl.ENVIRONMENT_IDENTIFIER,
				"Environment Identifier - \"test\", \"sit\", \"uat\", \"dev\" etc: null = prod");
		addProperty(ENVIRONMENT_STANZA_KEY + DISPLAY_DELIM + ENVIRONMENT_CUSTOMER_KEY, UtilImpl.CUSTOMER, "Customer - default customer for sign in page");
		addProperty(ENVIRONMENT_STANZA_KEY + DISPLAY_DELIM + ENVIRONMENT_MODULE_DIRECTORY_KEY, UtilImpl.MODULE_DIRECTORY,
				"Module Directory - 					Absolute path on the filesystem to the source directory where modules live \n" +
						"					for generating report templates and new documents via skyve script. \n" +
						"					This must be a valid /modules directory e.g. C:/Workspace/myapp/src/main/java/modules\n" +
						"					(note that if you are running on Windows do NOT use backslashes)					\n" +
						"");
		addProperty(ENVIRONMENT_STANZA_KEY + DISPLAY_DELIM + ENVIRONMENT_SUPPORT_EMAIL_ADDRESS_KEY, UtilImpl.SUPPORT_EMAIL_ADDRESS, "Support Email Address - Email address for system support");
		addProperty(ENVIRONMENT_STANZA_KEY + DISPLAY_DELIM + ENVIRONMENT_SHOW_SETUP_KEY, Boolean.toString(UtilImpl.SHOW_SETUP), "Show setup options on sign-in for DevOps users");

		originalValues().clear();
	}

	/**
	 * add a property to the startup configuration
	 * 
	 * @param name
	 * @param value
	 * @param description
	 * @throws Exception
	 */
	private void addProperty(String name, String value, String description) throws Exception {
		Generic property = Generic.newInstance();
		property.setText5001(name);
		property.setText5002(value);
		property.setText5003(description);
		getStartupProperties().add(property);

		// keep original value
		Generic propertyOriginal = Util.cloneToTransientBySerialization(property);
		getOriginalStartupProperties().add(propertyOriginal);
	}

	/**
	 * Apply changes to startup configuration
	 * and save the new settings to the .json file for next startup
	 * 
	 * The approach is to replace property values using regex.
	 * The json properties file is not strict JSON, and contains documentation to assist users.
	 * The approach of using regex attempts to leave documentation intact
	 * (included what might be added by users).
	 * 
	 * @throws Exception
	 */
	public void applyStartupConfiguration() throws Exception {

		validateStartupConfiguration();

		try {
			Path path = Paths.get(UtilImpl.PROPERTIES_FILE_PATH);
			Path nPath = Paths.get(UtilImpl.PROPERTIES_FILE_PATH);

			Charset charset = StandardCharsets.UTF_8;
			String content = new String(Files.readAllBytes(path), charset);
			Object oldValue = null;
			Object replacement = null;
			Map<String, Object> changed = new TreeMap<>();

			for (Generic g : getStartupProperties()) {

				String[] parts = g.getText5001().split("\\.");
				int index = getStartupProperties().indexOf(g);
				boolean replace = false;
				for (Generic gOld : getOriginalStartupProperties()) {
					if (gOld.getText5001().equals(g.getText5001())) {
						oldValue = gOld.getText5002();
						replace = true;
						break;
					}
				}
				replacement = g.getText5002();
				String stanza = null;
				String propertyName = null;
				boolean found = false;
				boolean quoted = true;

				if (parts.length == 1) {
					propertyName = parts[0];
				} else {
					stanza = parts[0];
					propertyName = parts[1];
				}

				if (!replace) {
					// insert new property

					quoted = true;
					found = true;
				} else if ((oldValue == null && replacement != null)
						|| (oldValue != null && !oldValue.equals(replacement))) {

					changed.put(g.getText5001(), g.getText5002());

					switch (g.getText5001()) {
					case SMTP_STANZA_KEY + DISPLAY_DELIM + SMTP_SERVER_KEY:
					case SMTP_STANZA_KEY + DISPLAY_DELIM + SMTP_UID_KEY:
					case SMTP_STANZA_KEY + DISPLAY_DELIM + SMTP_PWD_KEY:
					case SMTP_STANZA_KEY + DISPLAY_DELIM + SMTP_SENDER_KEY:
					case SMTP_STANZA_KEY + DISPLAY_DELIM + SMTP_TEST_RECIPIENT_KEY:
					case MAP_STANZA_KEY + DISPLAY_DELIM + MAP_TYPE_KEY:
					case ENVIRONMENT_STANZA_KEY + DISPLAY_DELIM + ENVIRONMENT_IDENTIFIER_KEY:
					case ENVIRONMENT_STANZA_KEY + DISPLAY_DELIM + ENVIRONMENT_CUSTOMER_KEY:
					case ENVIRONMENT_STANZA_KEY + DISPLAY_DELIM + ENVIRONMENT_MODULE_DIRECTORY_KEY:
					case ENVIRONMENT_STANZA_KEY + DISPLAY_DELIM + ENVIRONMENT_SUPPORT_EMAIL_ADDRESS_KEY:
						found = true;
						break;
					case SMTP_STANZA_KEY + DISPLAY_DELIM + SMTP_PORT_KEY:
					case SMTP_STANZA_KEY + DISPLAY_DELIM + SMTP_TEST_BOGUS_SEND_KEY:
					case ENVIRONMENT_STANZA_KEY + DISPLAY_DELIM + ENVIRONMENT_SHOW_SETUP_KEY:
						quoted = false;
						found = true;
						break;

					default:
						found = replace;
						quoted = true;
						break;
					}
				}

				if (found) {

					StringBuilder findExp = new StringBuilder();
					String replacementString = null;
					String oldValueString = null;
					if (replace) {

						if (quoted) {
							oldValueString = (oldValue == null || oldValue.toString().length() == 0 ? NULL_STRING : QUOTE + oldValue + QUOTE);
							replacementString = (replacement == null || replacement.toString().length() == 0 ? NULL_STRING : QUOTE + replacement + QUOTE);
						} else {
							oldValueString = (oldValue == null ? NULL_STRING : oldValue.toString());
							replacementString = (replacement == null ? NULL_STRING : replacement.toString());
						}

						// handle complex properties
						findExp.append(stanza).append("\\s*:"); // the stanza in the json
						if (parts.length > 2) {
							findExp.append("(.|\\n)*?\\n\\s*").append(propertyName);
							findExp.append(":\\s*(.|\\n)*?\\n\\s*").append(parts[2]).append(":\\s*");

						} else {
							findExp.append("(.|\n)*?(\n\\s*"); // new lines or spaces
							findExp.append(propertyName).append(":\\s*)"); // the property declaration
						}
						findExp.append("(").append(oldValueString).append(")"); // the value to replace

						// Util.LOGGER.info(findExp.toString());

						if (findExp.length() > 0) {
							try {
								content = replaceGroup(findExp.toString(), content, 3, replacementString);
							} catch (@SuppressWarnings("unused") StackOverflowError e) {
								// TODO: handle exception
								EXT.push(new PushMessage().user(CORE.getUser()).growl(MessageSeverity.error, g.getText5001() + " - The previous value could not be matched"));
								throw new ValidationException(
										new Message(Binder.createIndexedBinding(ControlPanel.startupPropertiesPropertyName, index), "The previous value could not be matched."));
							}
						}

					} else {

						// handle insertion of new API property

						// TODO - handle complex sub stanzas - this is not yet implemented
						// handle by inserting new keys at the beginning of the stanza rather than at the end

						StringBuilder sb = new StringBuilder();
						sb.append("\n\t\t").append(propertyName).append(": ");
						sb.append((g.getText5002() == null ? NULL_STRING : QUOTE + g.getText5002() + QUOTE));
						sb.append("\n\t");

						// special case - api: {} breaks the usual pattern replace
						// stuff a spacer between the braces
						findExp.append(API_STANZA_KEY + ":(\\s*)(\\{)(\\})");
						replacementString = " }";
						content = replaceGroup(findExp.toString(), content, 3, replacementString);

						// as the only api key
						// if there are other keys, this replace will do nothing
						findExp = new StringBuilder();
						findExp.append(API_STANZA_KEY + ":(\\s*)(\\{)(\\n|\\s)*?(\\})");
						replacementString = sb.toString();
						String replaced = replaceGroup(findExp.toString(), content, 3, replacementString);

						// if not yet inserted
						if (replaced.equals(content)) {
							// after existing api keys
							findExp = new StringBuilder();
							findExp.append(API_STANZA_KEY + ":(\\s*)(\\{)(.|\\n\\s)*?(\\})");
							replacementString = "," + sb.toString();
							content = replaceGroup(findExp.toString(), content, 3, replacementString);
						} else {
							content = replaced;
						}
					}

				}
			}

			Files.write(nPath, content.getBytes(charset));

			// now update the application state
			for (String k : changed.keySet()) {

				// put the changed value into the CONFIGURATION map
				putConfig(UtilImpl.CONFIGURATION, k, changed.get(k));

				switch (k) {
				case SMTP_STANZA_KEY + DISPLAY_DELIM + SMTP_SERVER_KEY:
					UtilImpl.SMTP = (String) changed.get(k);
					break;
				case SMTP_STANZA_KEY + DISPLAY_DELIM + SMTP_UID_KEY:
					UtilImpl.SMTP_UID = (String) changed.get(k);
					break;
				case SMTP_STANZA_KEY + DISPLAY_DELIM + SMTP_PWD_KEY:
					UtilImpl.SMTP_PWD = (String) changed.get(k);
					break;
				case SMTP_STANZA_KEY + DISPLAY_DELIM + SMTP_SENDER_KEY:
					UtilImpl.SMTP_SENDER = (String) changed.get(k);
					break;
				case SMTP_STANZA_KEY + DISPLAY_DELIM + SMTP_TEST_RECIPIENT_KEY:
					UtilImpl.SMTP_TEST_RECIPIENT = (String) changed.get(k);
					break;
				case SMTP_STANZA_KEY + DISPLAY_DELIM + SMTP_PORT_KEY:
						String port = (String) changed.get(k);
						UtilImpl.SMTP_PORT = port != null ? Integer.valueOf(port).intValue() : 25;
					break;
				case SMTP_STANZA_KEY + DISPLAY_DELIM + SMTP_TEST_BOGUS_SEND_KEY:
					UtilImpl.SMTP_TEST_BOGUS_SEND = Boolean.parseBoolean((String) changed.get(k));
					break;
				case MAP_STANZA_KEY + DISPLAY_DELIM + MAP_TYPE_KEY:
					String value = (String) changed.get(k);
					UtilImpl.MAP_TYPE = (value == null) ? null : MapType.valueOf(value);
					break;
				case ENVIRONMENT_STANZA_KEY + DISPLAY_DELIM + ENVIRONMENT_IDENTIFIER_KEY:
					UtilImpl.ENVIRONMENT_IDENTIFIER = (String) changed.get(k);
					break;
				case ENVIRONMENT_STANZA_KEY + DISPLAY_DELIM + ENVIRONMENT_CUSTOMER_KEY:
					UtilImpl.CUSTOMER = (String) changed.get(k);
					break;
				case ENVIRONMENT_STANZA_KEY + DISPLAY_DELIM + ENVIRONMENT_MODULE_DIRECTORY_KEY:
					UtilImpl.MODULE_DIRECTORY = (String) changed.get(k);
					break;
				case ENVIRONMENT_STANZA_KEY + DISPLAY_DELIM + ENVIRONMENT_SUPPORT_EMAIL_ADDRESS_KEY:
					UtilImpl.SUPPORT_EMAIL_ADDRESS = (String) changed.get(k);
					break;
				case ENVIRONMENT_STANZA_KEY + DISPLAY_DELIM + ENVIRONMENT_SHOW_SETUP_KEY:
					UtilImpl.SHOW_SETUP = Boolean.parseBoolean((String) changed.get(k));
					break;
				case API_STANZA_KEY + DISPLAY_DELIM + GOOGLE_MAPS_V3_KEY:
					UtilImpl.GOOGLE_MAPS_V3_API_KEY = (String) changed.get(k);
					break;
				case API_STANZA_KEY + DISPLAY_DELIM + GOOGLE_RECAPTCHA_SITE_KEY:
					UtilImpl.GOOGLE_RECAPTCHA_SITE_KEY = (String) changed.get(k);
					break;
				case API_STANZA_KEY + DISPLAY_DELIM + CK_EDITOR_CONFIG_FILE_URL:
					UtilImpl.CKEDITOR_CONFIG_FILE_URL = (String) changed.get(k);
					break;

				default:
					break;
				}
			}

			loadStartupConfiguration();

			EXT.push(new PushMessage().user(CORE.getUser()).growl(MessageSeverity.info, "Startup configuration changes have been applied"));

		} catch (ValidationException v) {
			throw v;
		} catch (Exception e) {
			throw e;
		}

	}

	/**
	 * Validate properties
	 * 
	 * Validate key properties where possible
	 * 
	 * @param bean
	 * @throws Exception
	 */
	private void validateStartupConfiguration() throws Exception {

		ValidationException v = new ValidationException();

		for (Generic g : getStartupProperties()) {
			String property = g.getText5001();
			String value = g.getText5002();
			int index = getStartupProperties().indexOf(g);
			if (property != null) {
				switch (property) {
				case ENVIRONMENT_STANZA_KEY + DISPLAY_DELIM + ENVIRONMENT_MODULE_DIRECTORY_KEY:
					if (value != null) {

						value = UtilImpl.cleanupModuleDirectory(value);

						File moduleDirectory = new File(value);
						if (!moduleDirectory.exists()) {
							v.getMessages().add(new Message(Binder.createIndexedBinding(ControlPanel.startupPropertiesPropertyName, index),
									property + " '" + value + "' does not exist."));
						}
						if (!moduleDirectory.isDirectory()) {
							value = UtilImpl.MODULE_DIRECTORY;
							v.getMessages().add(new Message(Binder.createIndexedBinding(ControlPanel.startupPropertiesPropertyName, index),
									ENVIRONMENT_STANZA_KEY + DISPLAY_DELIM + "moduleDirectory '" + value + "' is not a directory."));
						}
					}
					g.setText5002(value);
					break;
				case SMTP_STANZA_KEY + DISPLAY_DELIM + SMTP_PORT_KEY:
					// must be null or integer
					if (value != null) {
						try {
							int x = Integer.parseInt(value);
							g.setText5002(Integer.toString(x));
						} catch (@SuppressWarnings("unused") Exception ex) {
							v.getMessages().add(new Message(Binder.createIndexedBinding(ControlPanel.startupPropertiesPropertyName, index),
									property + " must be a valid integer."));
						}
					}
					break;
				case SMTP_STANZA_KEY + DISPLAY_DELIM + SMTP_TEST_BOGUS_SEND_KEY:
				case ENVIRONMENT_STANZA_KEY + DISPLAY_DELIM + ENVIRONMENT_SHOW_SETUP_KEY:
					// must be null, true or false
					if (value != null) {
						Message msg = new Message(Binder.createIndexedBinding(ControlPanel.startupPropertiesPropertyName, index),
								property + " must be either true or false.");
						try {
							if (value.toLowerCase().equals("true") || value.toLowerCase().equals("false")) {
								boolean x = Boolean.parseBoolean(value);
								g.setText5002(Boolean.toString(x));
							} else {
								v.getMessages().add(msg);
							}
						} catch (@SuppressWarnings("unused") Exception ex) {
							v.getMessages().add(msg);
						}
					}
					break;
				default:
					break;
				}
			}
		}

		// if any validation errors have occurred, throw them
		if (!v.getMessages().isEmpty()) {
			throw v;
		}
	}

	/**
	 * Replace a specified group within the source given the regex
	 * Reg exp handling for "json" startup configuration settings
	 * 
	 * @param regex
	 *            - the regular expression
	 * @param source
	 *            - the original string to change
	 * @param groupToReplace
	 *            - the number/index of the group to change
	 * @param replacement
	 *            - the replacement value to apply for the replaced group
	 * @return
	 */
	public static String replaceGroup(String regex, String source, int groupToReplace, String replacement) {
		return replaceGroup(regex, source, groupToReplace, 1, replacement);
	}

	public static String replaceGroup(String regex, String source, int groupToReplace, int groupOccurrence, String replacement) {
		Matcher m = Pattern.compile(regex).matcher(source);
		for (int i = 0; i < groupOccurrence; i++)
			if (!m.find())
				return source; // pattern not met, may also throw an exception here
		return new StringBuilder(source).replace(m.start(groupToReplace), m.end(groupToReplace), replacement).toString();
	}

	/**
	 * Puts the value into the CONFIG Map
	 * NOTE: recursive if the get of the key is itself a Map
	 */
	private static void putConfig(Map<String, Object> map, String key, Object putValue) {

		if (key != null && key.length() > 0) {
			String[] subKey = key.split("\\.");

			@SuppressWarnings("unchecked")
			Map<String, Object> valueMap = (Map<String, Object>) map.get(subKey[0]);
			if (valueMap != null) {
				Object value = valueMap.get(subKey[1]);
				if (value instanceof String || value instanceof Boolean) {
					valueMap.put(subKey[1], putValue);
					UtilImpl.CONFIGURATION.put(subKey[0], valueMap);
				} else if (value instanceof Map) {
					if (key.length() > (subKey[0].length() + 1)) {
						putConfig(valueMap, key.substring(subKey[0].length() + 1), putValue);
					}
				}
			}
		}
	}

}
