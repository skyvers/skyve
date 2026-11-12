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
import java.util.Objects;
import java.util.UUID;

import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.types.OptimisticLock;
import org.skyve.domain.types.converters.Converter;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.model.document.field.ConvertibleField;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.util.Binder.TargetMetaData;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Deserialises JSON text into Java objects: {@link java.util.Map}, {@link java.util.List},
 * {@link String}, {@link Number}, {@link Boolean}, and {@code null}.
 *
 * <p>Explicitly typed reads reconstruct class-free Java records by matching JSON
 * properties to record components and invoking the canonical constructor. The reader
 * also supports domain-aware reading: when a {@link org.skyve.metadata.user.User} and
 * document context are supplied, attribute values are converted via the document's
 * declared {@link org.skyve.domain.types.converters.Converter}.
 *
 * <p>Threading: not thread-safe. Create a new instance per parse operation.
 */
public class JSONReader {
	/**
	 * Selects how an object encountered in the JSON stream is interpreted.
	 *
	 * <p>{@link #auto} recognises Skyve documents and self-described Java objects.
	 * {@link #typed} recognises Skyve documents while leaving Java objects as maps for
	 * explicit type conversion. {@link #dynamic} leaves every object as a map. The
	 * remaining values describe the representation selected for an individual object.
	 */
	@SuppressWarnings("java:S115") // Suppress "Constant names should comply with a naming convention" as these are not constants but enum values
	public enum JSONMode {
		/** Infers the representation of every object from its first property. */
		auto,
		/** Reconstructs documents but retains Java objects for explicit type mapping. */
		typed,
		/** Represents objects as insertion-ordered maps. */
		dynamic,
		/** Reconstructs objects as Skyve document beans. */
		bean,
		/** Reconstructs objects as ordinary Java beans or records. */
		object;
	}

	// Punctuation tokens are compared by identity (==), not equals(), because string() always
	// allocates a new String - so a string value such as "]" is never mistaken for punctuation.
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
	private JSONMode mode = JSONMode.auto;
	private int stringLength;
	private CharacterIterator it;
	private char c;
	private Object token;
	private StringBuilder sb = new StringBuilder();

	/**
	 * Creates a reader with an optional Skyve user context.
	 *
	 * <p>A user is required to reconstruct document instances identified by
	 * {@code bizModule} and {@code bizDocument}. Without one, those objects remain
	 * dynamic maps.
	 *
	 * @param user current user, or {@code null} when document reconstruction is not required
	 */
	public JSONReader(@Nullable User user) {
		this.user = user;
		this.customer = (user == null) ? null : user.getCustomer();
	}

	/**
	 * Deserialises one JSON value, recognising self-described Java objects and Skyve
	 * documents when the required user context is available.
	 *
	 * <p>Side effects: may load a payload-named Java class and invoke its constructor,
	 * or instantiate a Skyve document through its metadata definition.
	 *
	 * @param string JSON text containing exactly one value; must not be {@code null}
	 * @return the reconstructed object, dynamic map/list, scalar value, or {@code null}
	 * @throws Exception if the JSON is malformed or an identified object cannot be reconstructed
	 */
	public Object read(@Nonnull String string) throws Exception {
		stringLength = string.length();
		it = new StringCharacterIterator(string);
		c = it.first();
		return read();
	}

	/**
	 * Deserialises JSON into an explicitly supplied Java type. Records are
	 * reconstructed from class-free JSON objects by matching component names and
	 * invoking the canonical constructor. Nested records, arrays, collections, and
	 * maps are converted recursively.
	 *
	 * @param <T> target type
	 * @param string JSON text
	 * @param type target Java type
	 * @return deserialised value, or {@code null} when the JSON value is null
	 * @throws Exception when the JSON is malformed or cannot be converted to the
	 *         target type
	 * @since 10.0
	 */
	public @Nullable <T> T read(@Nonnull String string, @Nonnull Class<T> type) throws Exception {
		JSONMode previousMode = mode;
		mode = JSONMode.typed;
		try {
			return JSONTypeMapper.convert(read(string), type);
		}
		finally {
			mode = previousMode;
		}
	}

	/**
	 * Advances the source iterator and stores its current character.
	 *
	 * <p>Side effects: updates the parser's current-character state.
	 *
	 * @return the next character, or {@link CharacterIterator#DONE} at end of input
	 */
	private char next() {
		c = it.next();
		return c;
	}

	/**
	 * Advances past consecutive whitespace characters.
	 *
	 * <p>Side effects: leaves the current-character state at the first non-whitespace
	 * character or {@link CharacterIterator#DONE}.
	 */
	private void skipWhiteSpace() {
		while (Character.isWhitespace(c)) {
			next();
		}
	}

	/**
	 * Reads the next token or complete JSON value from the current parser position.
	 *
	 * <p>Side effects: advances the source iterator and replaces the current token.
	 * Object and array tokens are parsed recursively.
	 *
	 * @return the parsed value or structural token, or {@code null} for JSON {@code null}
	 * @throws Exception if a nested value is malformed or cannot be reconstructed
	 */
	@SuppressWarnings({"java:S3776", "java:S6541"}) // Complexity OK
	private @Nullable Object read() throws Exception {
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
			else if (literal("false")) {
				token = Boolean.FALSE;
			}
			else if (literal("true")) {
				token = Boolean.TRUE;
			}
			else if (literal("null")) {
				token = null;
			}
			else {
				token = string('\0');
			}
		}
// Util.LOGGER.info("token: {}", token); // enable this line to see the token stream
		return token;
	}

	/**
	 * Consumes a {@code true}, {@code false} or {@code null} literal at the current position.
	 *
	 * <p>The literal must end there, so that an unquoted key such as {@code nullable} or
	 * {@code trueClientIp} is read as a key and not as the literal it starts with.
	 *
	 * <p>Side effects: advances past the literal when it matches, otherwise leaves the
	 * parser position unchanged.
	 *
	 * @param word the literal to match
	 * @return {@code true} if the literal was consumed
	 */
	private boolean literal(@Nonnull String word) {
		int start = it.getIndex();
		int length = word.length();
		int i = 0;
		while ((i < length) && (c == word.charAt(i))) {
			next();
			i++;
		}
		if ((i == length) && (! Character.isJavaIdentifierPart(c))) {
			return true;
		}
		c = it.setIndex(start);
		return false;
	}

	/**
	 * Reads an object body and selects its representation from the configured mode and
	 * first property.
	 *
	 * @return a Skyve document, Java object, record, or insertion-ordered dynamic map
	 * @throws Exception if the object is malformed or reconstruction fails
	 */
	@SuppressWarnings({"java:S3776", "java:S6541"}) // parser state establishes required keys
	private @Nullable Object object() throws Exception {
		Object key = read();
		JSONMode objectMode;
		if ((customer != null) && Bean.MODULE_KEY.equals(key)) {
			objectMode = JSONMode.bean;
		}
		else if ((mode == JSONMode.auto) && "class".equals(key)) {
			objectMode = JSONMode.object;
		}
		else {
			objectMode = JSONMode.dynamic;
		}

		if (objectMode == JSONMode.bean) {
			// Get module name
			read(); // should be a colon
			String moduleName = (String) read();
			if (moduleName == null) {
				throw new IllegalStateException("No moduleName in Skyve document bean");
			}
			read(); // should be a comma

			// Get document name
			key = read();
			if (! Bean.DOCUMENT_KEY.equals(key)) {
				throw new IllegalStateException("found key of " + key + " when expecting 'documentName'");
			}

			read(); // should be a colon
			String documentName = (String) read();
			if (documentName == null) {
				throw new IllegalStateException("No documentName in Skyve document bean");
			}
			Object separator = read();

			Module module = customer.getModule(moduleName);
			Document document = module.getDocument(customer, documentName);

			// Create bean instance
			Bean result = ((DocumentImpl) document).newInstance(customer);

			String propertyName = null;
			if (separator == COMMA) {
				propertyName = (String) read();
			}
			else if (separator != OBJECT_END) {
				throw new IllegalStateException("Malformed JSON object after bizDocument attribute");
			}

			int i = 0;
			while (token != OBJECT_END) {
				if (propertyName == null) {
					throw new IllegalStateException("No propertyName in Skyve document bean after a comma");
				}
				
				read(); // should be a colon
				if (token != OBJECT_END) {
					Object value = read();
					if (Bean.DOCUMENT_ID.equals(propertyName)) {
						try {
							UUID.fromString((String) value);
							BindUtil.set(result, propertyName, value);
						}
						catch (@SuppressWarnings("unused") Exception e) {
							// do nothing - ie leave the generated UUID in place
						}
					}
					else if (value instanceof List<?> list) {
						@SuppressWarnings("unchecked")
						List<Object> children = (List<Object>) BindUtil.get(result, propertyName);
						if (children == null) { // should never be
							throw new IllegalStateException(propertyName + " list in " + result + " is null - can't add " + value);
						}
						children.addAll(list);
					}
					else if (PersistentBean.LOCK_NAME.equals(propertyName)) {
						OptimisticLock lock = null;
						String lockString = (String) value;
						if ((lockString != null) && (! lockString.isEmpty())) {
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
						if (value instanceof String valueString) {
							if (valueString.isEmpty()) {
								value = null;
							}
							else {
								TargetMetaData target = BindUtil.getMetaDataForBinding(customer, 
																						module, 
																						document,
																						propertyName);
								Attribute attribute = target.getAttribute();
								if (attribute instanceof ConvertibleField convertibleField) {
									Converter<?> converter = convertibleField.getConverterForCustomer(customer);
									if (converter != null) {
										value = converter.fromDisplayValue(valueString);
									}
								}
							}
						}
						if (BindUtil.isMutable(result, propertyName)) {
							BindUtil.convertAndSet(result, propertyName, value);
						}
					}

					if (read() == COMMA) {
						propertyName = (String) read();
					}
				}
				// Defend infinite loop
				checkObjectLength(i++, propertyName);
			}

			// Set the bizCustomer, bizDataGroup and bizUser now that the bean is populated from JSON data
			result.setBizCustomer(customer.getName());
			if (result.isNotPersisted()) {
				result.setBizDataGroupId(user.getDataGroupId());
				result.setBizUserId(user.getId());
			}

			return result;
		}
		else if (objectMode == JSONMode.dynamic) {
			// Order can be important - like in constant range map expression
			Map<Object, Object> result = new LinkedHashMap<>();
			int i = 0;
			while (token != OBJECT_END) {
				read(); // should be a colon
				if (token != OBJECT_END) {
					result.put(key, read());
					if (read() == COMMA) {
						key = read();
					}
				}
				// Defend infinite loop
				checkObjectLength(i++, key);
			}

			return result;
		}

		return javaObject();
	}

	/**
	 * Reconstructs a Java object whose leading {@code class} property has already been read.
	 *
	 * <p>Records are created through their canonical constructor; mutable Java beans are
	 * created through a no-argument constructor and populated through Skyve binding.
	 *
	 * @return the reconstructed non-null Java object
	 * @throws Exception if the class cannot be loaded, constructed, converted, or populated
	 */
	private @Nonnull Object javaObject() throws Exception {
		read(); // should be a colon
		String className = (String) read();
		Class<?> type = Thread.currentThread().getContextClassLoader().loadClass(className);
		Map<String, Object> values = remainingObjectValues();
		if (type.isRecord()) {
			return Objects.requireNonNull(JSONTypeMapper.convert(values, type), "record");
		}

		Object result = type.getDeclaredConstructor().newInstance();
		populateJavaObject(result, values);
		return result;
	}

	/**
	 * Reads the properties following a leading Java {@code class} attribute.
	 *
	 * @return an insertion-ordered mutable map excluding the consumed class attribute
	 * @throws Exception if a property or nested value is malformed
	 */
	private @Nonnull Map<String, Object> remainingObjectValues() throws Exception {
		Map<String, Object> result = new LinkedHashMap<>();
		Object separator = read();
		if (separator == OBJECT_END) {
			return result;
		}
		if (separator != COMMA) {
			throw new IllegalStateException("Malformed JSON object after class attribute");
		}

		String propertyName = (String) read();
		int i = 0;
		while (token != OBJECT_END) {
			read(); // should be a colon
			if (token != OBJECT_END) {
				result.put(propertyName, read());
				if (read() == COMMA) {
					propertyName = (String) read();
				}
			}
			checkObjectLength(i++, propertyName);
		}
		return result;
	}

	/**
	 * Rejects an object body that has consumed more tokens than its source length.
	 *
	 * @param iteration number of object-loop iterations completed
	 * @param propertyName last property being parsed, or {@code null}
	 * @throws IllegalStateException if the object body is unterminated
	 */
	private void checkObjectLength(int iteration, @Nullable Object propertyName) {
		if (iteration > stringLength) {
			throw new IllegalStateException("Malformed JSON - unterminated object " + propertyName);
		}
	}

	/**
	 * Applies parsed properties to an ordinary mutable Java object.
	 *
	 * <p>Side effects: replaces the contents of existing collection and map properties
	 * and assigns scalar properties through {@link BindUtil}.
	 *
	 * @param result object to populate; must expose writable properties
	 * @param properties insertion-ordered parsed property values; must not be {@code null}
	 * @throws Exception if a property cannot be read, assigned, or mutated
	 */
	private static void populateJavaObject(@Nonnull Object result, @Nonnull Map<String, Object> properties) {
		for (Map.Entry<String, Object> property : properties.entrySet()) {
			String propertyName = property.getKey();
			Object value = property.getValue();
			if (value instanceof Collection<?> collection) {
				@SuppressWarnings("unchecked")
				Collection<Object> values = (Collection<Object>) BindUtil.get(result, propertyName);
				if (values == null) {
					throw new IllegalStateException(propertyName + " list in " + result + " is null - can't add " + value);
				}
				values.clear();
				values.addAll(collection);
			}
			else if (value instanceof Map<?, ?> map) {
				@SuppressWarnings("unchecked")
				Map<Object, Object> values = (Map<Object, Object>) BindUtil.get(result, propertyName);
				if (values == null) {
					throw new IllegalStateException(propertyName + " map in " + result + " is null - can't put " + value);
				}
				values.clear();
				values.putAll(map);
			}
			else {
				BindUtil.set(result, propertyName, value);
			}
		}
	}

	/**
	 * Reads an array body into a mutable, encounter-ordered list.
	 *
	 * @return a newly allocated list containing the parsed array elements
	 * @throws Exception if an element is malformed or cannot be reconstructed
	 * @throws IllegalStateException if the array is not terminated
	 */
	private @Nonnull Object array() throws Exception {
		List<Object> result = new ArrayList<>();
		Object value = read();
		int i = 0;
		while (token != ARRAY_END) {
			result.add(value);
			if (read() == COMMA) {
				value = read();
			}
			// Defend infinite loop
			if (i++ > stringLength) {
				throw new IllegalStateException("Malformed JSON - unterminated array");
			}
		}

		return result;
	}

	/**
	 * Reads a JSON number from the current character position.
	 *
	 * <p>Integral values are returned as {@link Long}; values containing a fraction or
	 * exponent are returned as {@link BigDecimal} to avoid binary floating-point loss.
	 *
	 * @return the parsed non-null numeric value
	 * @throws NumberFormatException if the number syntax is invalid
	 */
	private @Nonnull Object number() {
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

	/**
	 * Appends consecutive decimal digits to the shared token buffer.
	 *
	 * <p>Side effects: advances the source iterator to the first non-digit character.
	 *
	 * @return the number of digits appended
	 */
	private int addDigits() {
		int result;
		for (result = 0; Character.isDigit(c); ++result) {
			add();
		}
		return result;
	}

	/**
	 * Reads a quoted string or an unquoted token into the shared token buffer.
	 *
	 * <p>A zero delimiter reads until a colon and is used for permissive unquoted keys.
	 * Backslash escapes and Unicode escapes are decoded while reading.
	 *
	 * @param delimiter quote character, or zero for an unquoted key/token
	 * @return the decoded, non-null string
	 * @throws IllegalStateException if the string is not terminated
	 */
	@SuppressWarnings("java:S3776") // Complexity OK
	private @Nonnull Object string(char delimiter) {
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

	/**
	 * Appends a character to the shared token buffer and advances the source iterator.
	 *
	 * @param cc character to append
	 */
	private void add(char cc) {
		sb.append(cc);
		next();
	}

	/**
	 * Appends the current character to the token buffer and advances the iterator.
	 */
	private void add() {
		add(c);
	}

	/**
	 * Decodes the four hexadecimal digits following a JSON Unicode escape marker.
	 *
	 * <p>Side effects: advances the source iterator through all four digits.
	 *
	 * @return the decoded UTF-16 code unit
	 * @throws IllegalStateException if any of the four characters is not hexadecimal
	 */
	private char unicode() {
		int value = 0;
		for (int i = 0; i < 4; ++i) {
			switch (next()) {
			case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
				value = (value << 4) + c - '0';
				break;
			case 'a','b', 'c', 'd', 'e', 'f':
				value = (value << 4) + c - 'a' + 10;
				break;
			case 'A','B', 'C', 'D', 'E', 'F':
				value = (value << 4) + c - 'A' + 10;
				break;
			default:
				throw new IllegalStateException("Malformed JSON - invalid Unicode escape");
			}
		}

		return (char) value;
	}
}
