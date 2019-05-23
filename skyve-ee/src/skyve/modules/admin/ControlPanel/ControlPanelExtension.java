package modules.admin.ControlPanel;

import java.io.File;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
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
import org.skyve.util.Binder;
import org.skyve.util.PushMessage;
import org.skyve.util.Util;

import modules.admin.domain.ControlPanel;
import modules.admin.domain.Generic;

public class ControlPanelExtension extends ControlPanel {
	private static final long serialVersionUID = -6204655500999983605L;

	private String unescapedResults;

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
		unescapedResults = results;
		if (results == null) {
			super.setResults(null);
		} else {
			super.setResults('\n' + results.replace("{", "\\{").replace("<", "&lt;").replace(">", "&gt;"));
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
		addProperty("smtp.server", UtilImpl.SMTP, "Mail Server URL");
		addProperty("smtp.port", UtilImpl.SMTP_PORT, "Mail Server Port");
		addProperty("smtp.uid", UtilImpl.SMTP_UID, "Mail Server User Name");
		addProperty("smtp.pwd", UtilImpl.SMTP_PWD, "Mail server password");
		addProperty("smtp.sender", UtilImpl.SMTP_SENDER, "Sender header value");
		addProperty("smtp.testRecipient", UtilImpl.SMTP_TEST_RECIPIENT, "Test Recipient (all emails only sent to this email address)");
		addProperty("smtp.testBogusSend", Boolean.toString(UtilImpl.SMTP_TEST_BOGUS_SEND), "Test Bogus Send - Set true so emails are never sent, they are logged)");
		// TODO - handle extended properties
		// if (UtilImpl.SMTP_PROPERTIES != null) {
		// for (String k : UtilImpl.SMTP_PROPERTIES.keySet()) {
		// addProperty("smtp.properties." + k, UtilImpl.SMTP_PROPERTIES.get(k), null);
		// }
		// }

//		if (UtilImpl.GOOGLE_MAPS_V3_API_KEY != null) {
			addProperty("api.googleMapsV3Key", UtilImpl.GOOGLE_MAPS_V3_API_KEY,
					"Google Maps API Key - to obtain a Google maps API Key go to https://developers.google.com/maps/documentation/javascript/get-api-key");
//		}
//		if (UtilImpl.GOOGLE_RECAPTCHA_SITE_KEY != null) {
			addProperty("api.googleRecaptchaSiteKey", UtilImpl.GOOGLE_RECAPTCHA_SITE_KEY, "Google Recaptcha Site Key");
//		}
//		if (UtilImpl.CKEDITOR_CONFIG_FILE_URL != null) {
			addProperty("api.ckEditorConfigFileUrl", UtilImpl.CKEDITOR_CONFIG_FILE_URL, "CKEditor Config File URL");
//		}
			
		Map<String, Object> api = (Map<String, Object>) UtilImpl.CONFIGURATION.get("api");
		if (api != null) {
			for(String k: api.keySet()) {
				//ignore keys already loaded above
				if(!k.equals("googleMapsV3Key") && !k.equals("googleRecaptchaSiteKey") && !k.equals("ckEditorConfigFileUrl") ) {
					
					//load this new key value - may be a string, or a hashmap
					Object value = api.get(k);
					if(value instanceof String) {
						addProperty("api." + k, (String) value,"");
					} else {
						// multiple depth level stanzas not yet supported
						setAddKeyNotSupported(Boolean.FALSE);
						
						Map<String, String> keyValue = (Map<String, String>) api.get(k);
						for(String innerK: keyValue.keySet()) {
							Object innerV = keyValue.get(innerK);
							if(innerV instanceof String) {
								addProperty("api." + k + "." + innerK, (String) innerV,"");
							} else {
								throw new ValidationException(new Message("Configuration includes unsupported types - edit the json settings file directly"));
							}
						}
					}
				}
			}
		}
		

		addProperty("environment.identifier", UtilImpl.ENVIRONMENT_IDENTIFIER, "Environment Identifier - \"test\", \"sit\", \"uat\", \"dev\" etc: null = prod");
		addProperty("environment.customer", UtilImpl.CUSTOMER, "Customer - default customer for sign in page");
		addProperty("environment.moduleDirectory", UtilImpl.MODULE_DIRECTORY,
				"Module Directory - 					Absolute path on the filesystem to the source directory where modules live \n" +
						"					for generating report templates and new documents via skyve script. \n" +
						"					This must be a valid /modules directory e.g. C:/Workspace/myapp/src/main/java/modules\n" +
						"					(note that if you are running on Windows do NOT use backslashes)					\n" +
						"");
		addProperty("environment.supportEmailAddress", UtilImpl.SUPPORT_EMAIL_ADDRESS, "Support Email Address - Email address for system support");
		addProperty("environment.showSetup", Boolean.toString(UtilImpl.SHOW_SETUP), "Show setup options on sign-in for DevOps users");

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
			Path nPath = Paths.get(UtilImpl.PROPERTIES_FILE_PATH + "2");

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
					case "smtp.server":
					case "smtp.uid":
					case "smtp.pwd":
					case "smtp.sender":
					case "smtp.testRecipient":
					case "environment.identifier":
					case "environment.customer":
					case "environment.moduleDirectory":
					case "environment.supportEmailAddress":
						found = true;
						break;
					case "smtp.port":
					case "smtp.testBogusSend":
					case "environment.showSetup":
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
							oldValueString = (oldValue == null || oldValue.toString().length()==0 ? "null" : "\"" + oldValue + "\"");
							replacementString = (replacement == null || replacement.toString().length()==0 ? "null" : "\"" + replacement + "\"");
						} else {
							oldValueString = (oldValue == null ? "null" : oldValue.toString());
							replacementString = (replacement == null ? "null" : replacement.toString());
						}

						// handle complex properties
						findExp.append(stanza).append("\\s*:"); // the stanza in the json
						if(parts.length>2) {
							findExp.append("(.|\\n)*?\\n\\s*").append(propertyName);
							findExp.append(":\\s*(.|\\n)*?\\n\\s*").append(parts[2]).append(":\\s*");
							
						} else {
							findExp.append("(.|\n)*?(\n\\s*"); // new lines or spaces
							findExp.append(propertyName).append(":\\s*)"); // the property declaration
						}
						findExp.append("(").append(oldValueString).append(")"); // the value to replace

//						Util.LOGGER.info(findExp.toString());
						
						if (findExp.length() > 0) {
							try {
								content = replaceGroup(findExp.toString(), content, 3, replacementString);
							} catch (StackOverflowError e) {
								// TODO: handle exception
								EXT.push(new PushMessage().user(CORE.getUser()).growl(MessageSeverity.error, g.getText5001() + " - The previous value could not be matched"));
								throw new ValidationException(
										new Message(Binder.createIndexedBinding(ControlPanel.startupPropertiesPropertyName, index), "The previous value could not be matched."));
							}
						}

					} else {
						
						//handle insertion of new API property
						
						//TODO - handle complex sub stanzas - this is not yet implemented
						
						StringBuilder sb = new StringBuilder();
						sb.append("\n\t\t").append(propertyName).append(": ");
						sb.append((g.getText5002() == null ? "null" : "\"" + g.getText5002() + "\""));
						sb.append("\n\t");
						
						//special case - api: {} breaks the usual pattern replace
						// stuff a spacer between the braces
						findExp.append("api:(\\s*)(\\{)(\\})");
						replacementString = " }";
						content = replaceGroup(findExp.toString(), content, 3, replacementString);

						// as the only api key
						// if there are other keys, this replace will do nothing
						findExp = new StringBuilder();
						findExp.append("api:(\\s*)(\\{)(\\n|\\s)*?(\\})");
						replacementString = sb.toString();
						String replaced = replaceGroup(findExp.toString(), content, 3, replacementString);
						
						//if not yet inserted
						if(replaced.equals(content)) {
							// after existing api keys
							findExp = new StringBuilder();
							findExp.append("api:(\\s*)(\\{)(.|\\n\\s)*?(\\})");
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

				switch (k) {
				case "smtp.server":
					UtilImpl.SMTP = (String) changed.get(k);
					break;
				case "smtp.uid":
					UtilImpl.SMTP_UID = (String) changed.get(k);
					break;
				case "smtp.pwd":
					UtilImpl.SMTP_PWD = (String) changed.get(k);
					break;
				case "smtp.sender":
					UtilImpl.SMTP_SENDER = (String) changed.get(k);
					break;
				case "smtp.testRecipient":
					UtilImpl.SMTP_TEST_RECIPIENT = (String) changed.get(k);
					break;
				case "environment.identifier":
					UtilImpl.ENVIRONMENT_IDENTIFIER = (String) changed.get(k);
					break;
				case "environment.customer":
					UtilImpl.CUSTOMER = (String) changed.get(k);
					break;
				case "environment.moduleDirectory":
					UtilImpl.MODULE_DIRECTORY = (String) changed.get(k);
					break;
				case "environment.supportEmailAddress":
					UtilImpl.SUPPORT_EMAIL_ADDRESS = (String) changed.get(k);
					break;
				case "smtp.port":
					UtilImpl.SMTP_PORT = (String) changed.get(k);
					break;
				case "smtp.testBogusSend":
					UtilImpl.SMTP_TEST_BOGUS_SEND = Boolean.parseBoolean((String) changed.get(k));
					break;
				case "environment.showSetup":
					UtilImpl.SHOW_SETUP = Boolean.parseBoolean((String) changed.get(k));
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
				case "environment.moduleDirectory":
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
									"environment.moduleDirectory '" + value + "' is not a directory."));
						}
					}
					g.setText5002(value);
					break;
				case "smtp.port":
					// must be null or integer
					if (value != null) {
						try {
							int x = Integer.parseInt(value);
							g.setText5002(Integer.toString(x));
						} catch (Exception ex) {
							v.getMessages().add(new Message(Binder.createIndexedBinding(ControlPanel.startupPropertiesPropertyName, index),
									property + " must be a valid integer."));
						}
					}
					break;
				case "smtp.testBogusSend":
				case "environment.showSetup":
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
						} catch (Exception ex) {
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

}
