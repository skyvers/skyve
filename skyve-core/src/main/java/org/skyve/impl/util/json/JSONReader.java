package org.skyve.impl.util.json;

import java.math.BigDecimal;
import java.text.CharacterIterator;
import java.text.StringCharacterIterator;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.types.OptimisticLock;
import org.skyve.domain.types.converters.Converter;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.model.document.field.ConvertableField;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.util.Binder.TargetMetaData;

public class JSONReader {
	public enum JSONMode {
		dynamic, // make collections and maps
		bean, // make document beans
		object; // make java beans
	}

	private static final String OBJECT_END = "}";
	private static final String ARRAY_END = "]";
	private static final String COLON = ":";
	private static final String COMMA = ",";

	private static Map<Character, Character> escapes = new HashMap<>();
	static {
		escapes.put(Character.valueOf('"'), Character.valueOf('"'));
		escapes.put(Character.valueOf('\\'), Character.valueOf('\\'));
		escapes.put(Character.valueOf('/'), Character.valueOf('/'));
		escapes.put(Character.valueOf('b'), Character.valueOf('\b'));
		escapes.put(Character.valueOf('f'), Character.valueOf('\f'));
		escapes.put(Character.valueOf('n'), Character.valueOf('\n'));
		escapes.put(Character.valueOf('r'), Character.valueOf('\r'));
		escapes.put(Character.valueOf('t'), Character.valueOf('\t'));
	}

	private User user;
	private Customer customer;
	private JSONMode mode = JSONMode.bean;
	private int stringLength;
	private CharacterIterator it;
	private char c;
	private Object token;
	private StringBuilder sb = new StringBuilder();

	public JSONReader(User user) {
		this.user = user;
		this.customer = (user == null) ? null : user.getCustomer();
	}

	public Object read(String string) throws Exception {
		stringLength = string.length();
		it = new StringCharacterIterator(string);
		c = it.first();
		return read();
	}

	private char next() {
		c = it.next();
		return c;
	}

	private void skipWhiteSpace() {
		while (Character.isWhitespace(c)) {
			next();
		}
	}

	private Object read() throws Exception {
		skipWhiteSpace();
		char ch = c;
		next();
		switch (ch) {
		case '"':
			token = string('"');
			break;
		case '\'':
			token = string('\'');
			break;
		case '[':
			token = array();
			break;
		case ']':
			token = ARRAY_END;
			break;
		case ',':
			token = COMMA;
			break;
		case '{':
			token = object();
			break;
		case '}':
			token = OBJECT_END;
			break;
		case ':':
			token = COLON;
			break;
		default:
			c = it.previous();
			if (Character.isDigit(c) || c == '-') {
				token = number();
			}
			else {
				if (c == 'f') {
					char a = next();
					char l = next();
					char s = next();
					char e = next();
					if ((a == 'a') && (l == 'l') && (s == 's') && (e == 'e')) {
						token = Boolean.FALSE;
						c = it.next();
					}
					else {
						it.previous();
						it.previous();
						it.previous();
						c = it.previous();
						token = string('\0');
					}
				}
				else if (c == 't') {
					char r = next();
					char u = next();
					char e = next();
					if ((r == 'r') && (u == 'u') && (e == 'e')) {
						token = Boolean.TRUE;
						c = it.next();
					}
					else {
						it.previous();
						it.previous();
						c = it.previous();
						token = string('\0');
					}
				}
				else if (ch == 'n') {
					char u = next();
					char l = next();
					char nextL = next();
					if ((u == 'u') && (l == 'l') && (nextL == 'l')) {
						token = null;
						c = it.next();
					}
					else {
						it.previous();
						it.previous();
						c = it.previous();
						token = string('\0');
					}
				}
				else {
					token = string('\0');
				}
			}
		}
// Util.LOGGER.info("token: " + token); // enable this line to see the token stream
		return token;
	}

	@SuppressWarnings("unchecked")
	private Object object() throws Exception {
		Object key = read();
		if (Bean.MODULE_KEY.equals(key)) {
			mode = JSONMode.bean;
		}
		else if ("class".equals(key)) {
			mode = JSONMode.object;
		}
		else {
			mode = JSONMode.dynamic;
		}

		if (mode == JSONMode.bean) {
			// Get module name
			read(); // should be a colon
			String moduleName = (String) read();
			read(); // should be a comma

			// Get document name
			key = read();
			if (! Bean.DOCUMENT_KEY.equals(key)) {
				// TODO propagate exceptions properly
				throw new IllegalStateException("found key of " + key + " when expecting 'documentName'");
			}

			read(); // should be a colon
			String documentName = (String) read();
			read(); // should be a comma

			Module module = customer.getModule(moduleName);
			Document document = module.getDocument(customer, documentName);

			// Create bean instance
			Bean result = ((DocumentImpl) document).newInstance(customer);

			String propertyName = (String) read();
			int i = 0;
			while (! OBJECT_END.equals(token)) {
				read(); // should be a colon
				if (! OBJECT_END.equals(token)) {
					Object value = read();
					if (Bean.DOCUMENT_ID.equals(propertyName)) {
						try {
							UUID.fromString((String) value);
							BindUtil.set(result, propertyName, value);
						}
						catch (@SuppressWarnings("unused") Exception e) {
							// do nothing - ie leave the generate UUID in place
						}
					}
					else if (value instanceof List) {
						List<Object> children = (List<Object>) BindUtil.get(result, propertyName);
						if (children == null) { // should never be
							throw new IllegalStateException(propertyName + " list in " + result + " is null - can't add " + value);
						}
						children.addAll((List<Object>) value);
					}
					else if (PersistentBean.LOCK_NAME.equals(propertyName)) {
						OptimisticLock lock = null;
						String lockString = (String) value;
						if ((lockString != null) && (lockString.length() > 0)) {
							lock = new OptimisticLock(lockString);
						}
						BindUtil.set(result, propertyName, lock);
					}
					else if (PersistentBean.VERSION_NAME.equals(propertyName)) {
						// Convert the number to an Integer
						if (value != null) {
							value = Integer.valueOf(((Number) value).intValue());
						}
						BindUtil.set(result, propertyName, value);
					}
					else {
						// Convert the value if required
						if (value instanceof String) {
							String valueString = (String) value;
							if (valueString.length() == 0) {
								value = null;
							}
							else {
								TargetMetaData target = BindUtil.getMetaDataForBinding(customer, 
																						module, 
																						document,
																						propertyName);
								if (target != null) {
									Attribute attribute = target.getAttribute();
									if (attribute instanceof ConvertableField) {
										Converter<?> converter = ((ConvertableField) attribute).getConverterForCustomer(customer);
										if (converter != null) {
											value = converter.fromDisplayValue(valueString);
										}
									}
								}
							}
						}
						if (BindUtil.isMutable(result, propertyName)) {
							BindUtil.convertAndSet(result, propertyName, value);
						}
					}

					if (COMMA.equals(read())) {
						propertyName = (String) read();
					}
				}
				// Defend infinite loop
				if (i++ > stringLength) {
					throw new IllegalStateException("Malformed JSON - unterminated object " + propertyName);
				}
			}

			// Set the bizCustomer, bizDataGroup and bizUser now that the bean is populated from JSON data
			result.setBizCustomer(customer.getName());
			if (result.isNotPersisted()) {
				result.setBizDataGroupId(user.getDataGroupId());
				result.setBizUserId(user.getId());
			}

			return result;
		}
		else if (mode == JSONMode.dynamic) {
			// Order can be important - like in constant range map expression
			Map<Object, Object> result = new LinkedHashMap<>();
			int i = 0;
			while (! OBJECT_END.equals(token)) {
				read(); // should be a colon
				if (! OBJECT_END.equals(token)) {
					result.put(key, read());
					if (COMMA.equals(read())) {
						key = read();
					}
				}
				// Defend infinite loop
				if (i++ > stringLength) {
					throw new IllegalStateException("Malformed JSON - unterminated object " + key);
				}
			}

			return result;
		}
		else if (mode == JSONMode.object) {
			// Get the class name
			read(); // should be a colon
			String className = (String) read();
			read(); // should be a comma

			Class<?> type = Thread.currentThread().getContextClassLoader().loadClass(className);
			Object result = type.getDeclaredConstructor().newInstance();

			String propertyName = (String) read();
			int i = 0;
			while (! OBJECT_END.equals(token)) {
				read(); // should be a colon
				if (! OBJECT_END.equals(token)) {
					// Util.LOGGER.info(result + " : " + propertyName);
					Object value = read();
					if (value instanceof Collection) {
						Collection<Object> values = (Collection<Object>) BindUtil.get(result, propertyName);
						if (values == null) { // should never be
							throw new IllegalStateException(propertyName + " list in " + result + " is null - can't add " + value);
						}
						values.clear();
						values.addAll((Collection<Object>) value);
					}
					else if (value instanceof Map) {
						Map<Object, Object> values = (Map<Object, Object>) BindUtil.get(result, propertyName);
						if (values == null) { // should never be
							throw new IllegalStateException(propertyName + " map in " + result + " is null - can't put " + value);
						}
						values.clear();
						values.putAll((Map<Object, Object>) value);
					}
					else {
						BindUtil.set(result, propertyName, value);
					}
				}

				if (COMMA.equals(read())) {
					propertyName = (String) read();
				}
				// Defend infinite loop
				if (i++ > stringLength) {
					throw new IllegalStateException("Malformed JSON - unterminate object " + propertyName);
				}
			}

			return result;
		}

		// should never be reached
		return null;
	}

	private Object array() throws Exception {
		List<Object> result = new ArrayList<>();
		Object value = read();
		int i = 0;
		while (! ARRAY_END.equals(token)) {
			result.add(value);
			if (COMMA.equals(read())) {
				value = read();
			}
			// Defend infinite loop
			if (i++ > stringLength) {
				throw new IllegalStateException("Malformed JSON - unterminated array");
			}
		}

		return result;
	}

	private Object number() {
		boolean isFloatingPoint = false;
		sb.setLength(0);

		if (c == '-') {
			add();
		}
		addDigits();
		if (c == '.') {
			add();
			addDigits();
			isFloatingPoint = true;
		}
		if (c == 'e' || c == 'E') {
			add();
			if (c == '+' || c == '-') {
				add();
			}
			addDigits();
			isFloatingPoint = true;
		}

		String s = sb.toString();
		return isFloatingPoint ? new BigDecimal(s) : Long.valueOf(s);
		// ? (length < 17) ? (Object)Double.valueOf(s) : new BigDecimal(s)
		// : (length < 19) ? (Object)Long.valueOf(s) : new BigInteger(s);
	}

	private int addDigits() {
		int result;
		for (result = 0; Character.isDigit(c); ++result) {
			add();
		}
		return result;
	}

	private Object string(char delimiter) {
		sb.setLength(0);
		int i = 0;
		while (((delimiter == '\0') && (c != ':')) || 
				((delimiter == '"') && (c != '"')) || 
				((delimiter == '\'') && (c != '\''))) {
			if (c == '\\') {
				next();
				if (c == 'u') {
					add(unicode());
				}
				else {
					Object value = escapes.get(Character.valueOf(c));
					if (value != null) {
						add(((Character) value).charValue());
					}
				}
			}
			else {
				add();
			}
			// Defend infinite loop
			if (i++ > stringLength) {
				throw new IllegalStateException("Malformed JSON - unterminated string " + sb);
			}
		}
		if (c != ':') {
			next(); // cleanup the ' or "
		}

		return sb.toString();
	}

	private void add(char cc) {
		sb.append(cc);
		next();
	}

	private void add() {
		add(c);
	}

	private char unicode() {
		int value = 0;
		for (int i = 0; i < 4; ++i) {
			switch (next()) {
			case '0':
			case '1':
			case '2':
			case '3':
			case '4':
			case '5':
			case '6':
			case '7':
			case '8':
			case '9':
				value = (value << 4) + c - '0';
				break;
			case 'a':
			case 'b':
			case 'c':
			case 'd':
			case 'e':
			case 'f':
				value = (value << 4) + c - 'k';
				break;
			case 'A':
			case 'B':
			case 'C':
			case 'D':
			case 'E':
			case 'F':
				value = (value << 4) + c - 'K';
				break;
			default:
			}
		}

		return (char) value;
	}
}