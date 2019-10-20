package org.skyve.util.test;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.UUID;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.GeometryFactory;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.types.Decimal;
import org.skyve.domain.types.Decimal2;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.model.document.AssociationImpl;
import org.skyve.impl.metadata.model.document.field.Decimal10;
import org.skyve.impl.metadata.model.document.field.Decimal5;
import org.skyve.impl.metadata.model.document.field.LengthField;
import org.skyve.impl.metadata.model.document.field.LongInteger;
import org.skyve.impl.metadata.model.document.field.Text;
import org.skyve.impl.metadata.model.document.field.TextFormat;
import org.skyve.impl.metadata.model.document.field.validator.DecimalValidator;
import org.skyve.impl.metadata.model.document.field.validator.IntegerValidator;
import org.skyve.impl.metadata.model.document.field.validator.LongValidator;
import org.skyve.impl.metadata.model.document.field.validator.TextValidator.ValidatorType;
import org.skyve.impl.persistence.AbstractDocumentQuery;
import org.skyve.impl.util.TimeUtil;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Collection;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.DocumentQuery.AggregateFunction;
import org.skyve.util.Binder;
import org.skyve.util.Util;

import com.mifmif.common.regex.Generex;

public class TestUtil {

	private static final Random RANDOM = new Random();
	private static final String NUMBERS = "0123456789";
	private static final String LETTERS = "abcdefghijklmnopqrstuvwxyz";
	private static final String ALPHA_NUMERIC = LETTERS + NUMBERS;

	/**
	 * Name of generic text file to fill long text fields and memos.
	 */
	private static final String LOREM = "lorem.txt";

	/**
	 * Cache of module.document.attributeName to loaded file random values.
	 */
	private static final Map<String, List<String>> DATA_CACHE = new HashMap<>();

	/**
	 * Cache of module.document.attributeName to @DataMap fileNames.
	 */
	private static final Map<String, String> DATA_MAP_CACHE = new HashMap<>();

	private TestUtil() {
		// no implementation
	}

	/**
	 * Make an instance of a document bean with random values for its properties.
	 * 
	 * @param <T> The type of Document bean to produce.
	 * @param user
	 * @param module
	 * @param document The document (corresponds to type T)
	 * @param depth How far to traverse the object graph - through associations and collections.
	 *        There are relationships that are never ending - ie Contact has Interactions which has User which has COntact
	 * @return The randomly constructed bean.
	 * @throws Exception
	 */
	public static <T extends Bean> T constructRandomInstance(User user,
			Module module,
			Document document,
			int depth)
			throws Exception {
		return TestUtil.constructRandomInstance(user, module, document, 1, depth);
	}

	/**
	 * Update an attribute on the given bean with a random value. Note: this method will
	 * not make use of any {@link DataMap} annotations present on the bean Factory. Please use
	 * {@link #updateAttribute(Module, Document, PersistentBean, Attribute)} instead.
	 * 
	 * @param bean The bean containing the to update
	 * @param attribute The current value of the attribute of the bean to modify
	 * @return The bean with a modified attribute with a different random value if possible
	 * @throws IOException
	 */
	public static <T extends PersistentBean> T updateAttribute(final T bean, final Attribute attribute) throws IOException {
		return updateAttribute(null, null, bean, attribute);
	}

	/**
	 * Update an attribute on the given bean with a random value
	 * 
	 * @param module The module (corresponds to type T)
	 * @param document The document (corresponds to type T)
	 * @param bean The bean containing the to update
	 * @param attribute The current value of the attribute of the bean to modify
	 * @return The bean with a modified attribute with a different random value if possible
	 * @throws IOException
	 */
	@SuppressWarnings({ "unchecked", "boxing" })
	public static <T extends PersistentBean> T updateAttribute(final Module module, final Document document, final T bean,
			final Attribute attribute) throws IOException {
		if (attribute == null) {
			return bean;
		}
		
		final String name = attribute.getName();
		final AttributeType type = attribute.getAttributeType();

		switch (type) {
			case bool:
				// get the current value of the boolean
				Boolean currentBool = (Boolean) Binder.get(bean, name);

				BindUtil.set(bean, name, currentBool != null ? !currentBool : false);
				break;
			case colour:
				BindUtil.set(bean, name, "#FFFFFF");
				break;
			case date:
				Date futureDate = new Date();
				TimeUtil.addDays(futureDate, RANDOM.nextInt(10) + 1);
				BindUtil.convertAndSet(bean, name, futureDate);
				break;
			case dateTime:
			case time:
			case timestamp:
				Date futureTime = new Date();
				TimeUtil.addHours(futureTime, RANDOM.nextInt(10) + 1);
				BindUtil.convertAndSet(bean, name, futureTime);
				break;
			case decimal10:
			case decimal2:
			case decimal5:
				BindUtil.convertAndSet(bean, name, randomDecimal(attribute));
				break;
			case integer:
			case longInteger:
				BindUtil.convertAndSet(bean, name, randomInteger(attribute));
				break;
			case enumeration:
				// get the current int value of the enum
				Class<Enum<?>> clazz = (Class<Enum<?>>) Binder.getPropertyType(bean, name);
				Object o = Binder.get(bean, name);
				Integer currentEnum = null;
				for (int i = 0; i < clazz.getEnumConstants().length; i++) {
					if (clazz.getEnumConstants()[i].equals(o)) {
						currentEnum = Integer.valueOf(i);
						break;
					}
				}
				// pick a new random enum
				BindUtil.set(bean, name, randomEnum(clazz, currentEnum));
				break;
			case geometry:
				BindUtil.set(bean, name, new GeometryFactory().createPoint(
						new Coordinate(RANDOM.nextInt(10), RANDOM.nextInt(10))));
				break;
			case id:
				BindUtil.set(bean, name, UUID.randomUUID().toString());
				break;
			case markup:
			case memo:
			case text:
				BindUtil.set(bean, name, randomText(module, document, attribute));
				break;
			case association:
			case collection:
			case content:
			case inverseMany:
			case inverseOne:
				break;
			default:
				break;
		}

		return bean;
	}

	/**
	 * Creates a cache key for an attribute so it is unique per document.
	 */
	private static String attributeKey(final Module module, final Document document, final String attributeName) {
		if (attributeName != null) {
			if (module != null && document != null) {
				return String.format("%s.%s.%s", module.getName(), document.getName(), attributeName);
			}
			return attributeName;
		}
		return null;
	}

	@SuppressWarnings("incomplete-switch") // content type missing from switch statement
	private static <T extends Bean> T constructRandomInstance(User user,
			Module module,
			Document document,
			int currentDepth,
			int maxDepth)
			throws Exception {
		T result = document.newInstance(user);

		for (Attribute attribute : document.getAllAttributes()) {
			String name = attribute.getName();
			AttributeType type = attribute.getAttributeType();

			switch (type) {
				case association:
					if (currentDepth < maxDepth) {
						AssociationImpl association = (AssociationImpl) attribute;
						Module associationModule = module;
						String associationModuleRef = module.getDocumentRefs().get(association.getDocumentName())
								.getReferencedModuleName();
						if (associationModuleRef != null) {
							associationModule = user.getCustomer().getModule(associationModuleRef);
						}
						Document associationDocument = associationModule.getDocument(user.getCustomer(),
								association.getDocumentName());
						BindUtil.set(result,
								name,
								TestUtil.constructRandomInstance(user,
										associationModule,
										associationDocument,
										currentDepth + 1,
										maxDepth));
					}
					break;
				case bool:
					// Random bools always are set to false as most processing changes around the true value.
					// This is considered the standard case, and can be set true after the random instance is constructed if needed.
					BindUtil.set(result, name, Boolean.FALSE);
					break;
				case collection:
					if (currentDepth < maxDepth) {
						Collection collection = (Collection) attribute;
						Module collectionModule = module;
						String collectionModuleRef = module.getDocumentRefs().get(collection.getDocumentName())
								.getReferencedModuleName();
						if (collectionModuleRef != null) {
							collectionModule = user.getCustomer().getModule(collectionModuleRef);
						}
						Document collectionDocument = collectionModule.getDocument(user.getCustomer(),
								collection.getDocumentName());
						@SuppressWarnings("unchecked")
						List<Bean> list = (List<Bean>) BindUtil.get(result, name);
						list.add(TestUtil.constructRandomInstance(user,
								collectionModule,
								collectionDocument,
								currentDepth + 1,
								maxDepth));
						list.add(TestUtil.constructRandomInstance(user,
								collectionModule,
								collectionDocument,
								currentDepth + 1,
								maxDepth));
					}
					break;
				case colour:
					BindUtil.set(result, name, "#FFFFFF");
					break;
				case date:
				case dateTime:
				case time:
				case timestamp:
					BindUtil.convertAndSet(result, name, new Date());
					break;
				case decimal10:
				case decimal2:
				case decimal5:
					BindUtil.convertAndSet(result, name, randomDecimal(attribute));
					break;
				case integer:
				case longInteger:
					BindUtil.convertAndSet(result, name, randomInteger(attribute));
					break;
				case enumeration:
					// pick a random value from the enum
					@SuppressWarnings("unchecked")
					Class<Enum<?>> clazz = (Class<Enum<?>>) Binder.getPropertyType(result, name);
					BindUtil.set(result, name, randomEnum(clazz, null));
					break;
				case geometry:
					BindUtil.set(result, name, new GeometryFactory().createPoint(new Coordinate(0, 0)));
					break;
				case id:
					BindUtil.set(result, name, UUID.randomUUID().toString());
					break;
				case markup:
				case memo:
				case text:
					BindUtil.set(result, name, randomText(user.getCustomerName(), module, document, attribute));
					break;
			}
		}
		return result;
	}

	/**
	 * Checks if the requested filename has a file extension.
	 * @param filename The name of the file to check
	 * @return True if the string contains a period followed by at least one character, false otherwise
	 */
	private static boolean hasExtension(final String filename) {
		if(filename != null && filename.length() > 0 && filename.indexOf(".") > 0) {
			if ((filename.substring(filename.indexOf(".") + 1)).length() > 0) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Returns a random decimal which conforms to any min and max validators set,
	 * otherwise between 0 and 10,000.
	 * 
	 * @param attribute The attribute to generate the random decimal for
	 * @return A random decimal
	 */
	public static Decimal randomDecimal(Attribute attribute) {
		Decimal min = new Decimal2(0), max = new Decimal2(10000);

		DecimalValidator validator = null;
		if (attribute instanceof org.skyve.impl.metadata.model.document.field.Decimal2) {
			org.skyve.impl.metadata.model.document.field.Decimal2 field = (org.skyve.impl.metadata.model.document.field.Decimal2) attribute;
			validator = field.getValidator();
		} else if (attribute instanceof Decimal5) {
			Decimal5 field = (Decimal5) attribute;
			validator = field.getValidator();
		} else if (attribute instanceof Decimal10) {
			Decimal10 field = (Decimal10) attribute;
			validator = field.getValidator();
		}

		if (validator != null) {
			if (validator.getMin() != null) {
				min = validator.getMin();
			}
			if (validator.getMax() != null) {
				max = validator.getMax();
			}
		}

		return new Decimal2(RANDOM.nextInt(
				(max.subtract(min))
						.add(new Decimal2(1)).intValue())).add(min);
	}

	public static String randomEmail(int length) {
		int addressLength = (int) Math.floor((length - 2) / 2);
		int domainLength = (int) Math.floor((length - 2) / 2) - 2;

		char[] address = new char[addressLength];
		for (int i = 0; i < addressLength; i++) {
			address[i] = Character.toChars(65 + (int) (RANDOM.nextDouble() * 26))[0];
		}

		char[] domain = new char[domainLength];
		for (int i = 0; i < domainLength; i++) {
			domain[i] = Character.toChars(65 + (int) (RANDOM.nextDouble() * 26))[0];
		}

		char[] code = new char[2];
		for (int i = 0; i < 2; i++) {
			code[i] = Character.toChars(65 + (int) (RANDOM.nextDouble() * 26))[0];
		}

		return String.valueOf(address) + "@" + String.valueOf(domain) + "." + String.valueOf(code);
	}

	/**
	 * Returns a random value from the enum class
	 * 
	 * @param clazz The enum class
	 * @param currentValue The current int value of the enum so that it is not chosen again
	 * @return A random enum constant
	 */
	@SuppressWarnings("boxing")
	public static <T extends Enum<?>> T randomEnum(Class<T> clazz, Integer currentValue) {
		int x;
		if (currentValue != null) {
			int currentValueInt = currentValue.intValue();
			do {
				x = RANDOM.nextInt(clazz.getEnumConstants().length);
			} while (x == currentValueInt);
		} else {
			x = RANDOM.nextInt(clazz.getEnumConstants().length);
		}

		return clazz.getEnumConstants()[x];
	}

	/**
	 * Returns a random string which complies to the format mask
	 * of the text attribute
	 * 
	 * @param textFormat The format to comply to
	 * @param length The maximum length of the random string
	 * @return A format compliant random string
	 */
	private static String randomFormat(TextFormat textFormat, int length) {

		String mask = textFormat.getMask();
		String out = new String();

		if (mask != null) {
			for (int i = 0; i < mask.length(); i++) {
				char c = mask.charAt(i);
				switch (c) {
					case '#':
						out += NUMBERS.charAt(RANDOM.nextInt(NUMBERS.length()));
						break;
					case 'A':
						out += ALPHA_NUMERIC.charAt(RANDOM.nextInt(ALPHA_NUMERIC.length()));
						break;
					case 'L':
						out += LETTERS.charAt(RANDOM.nextInt(LETTERS.length()));
						break;
					default:
						out += c;
						break;
				}
			}
		} else if (textFormat.getCase() != null) {
			out = randomString(RANDOM.nextInt(length) + 1);
			switch (textFormat.getCase()) {
				case capital:
					out = StringUtils.capitalize(out);
					break;
				case lower:
					out = out.toLowerCase();
					break;
				case upper:
					out = out.toUpperCase();
					break;
				default:
					break;
			}
		}

		if (out.length() > length) {
			out = StringUtils.left(out, length);
		}

		return out;
	}

	/**
	 * Returns a random number which conforms to any min and max validators set,
	 * otherwise between 0 and 10,000.
	 * 
	 * @param attribute The attribute to generate the random integer for
	 * @return A random integer
	 */
	public static Integer randomInteger(Attribute attribute) {
		int min = 0, max = 10000;

		// if there is a min and max make sure it is within the range
		if (attribute instanceof org.skyve.impl.metadata.model.document.field.Integer) {
			org.skyve.impl.metadata.model.document.field.Integer field = (org.skyve.impl.metadata.model.document.field.Integer) attribute;
			IntegerValidator validator = field.getValidator();
			if (validator != null) {
				if (validator.getMin() != null) {
					min = validator.getMin().intValue();
				}
				if (validator.getMax() != null) {
					max = validator.getMax().intValue();
				}
			}
		} else if (attribute instanceof org.skyve.impl.metadata.model.document.field.LongInteger) {
			LongInteger field = (LongInteger) attribute;
			LongValidator validator = field.getValidator();
			if (validator != null) {
				if (validator.getMin() != null) {
					min = validator.getMin().intValue();
				}
				if (validator.getMax() != null) {
					max = validator.getMax().intValue();
				}
			}
		}

		return new Integer(RANDOM.nextInt((max - min) + 1) + min);
	}

	/**
	 * Returns a random string which complies to the regular
	 * expression of the text attribute. Returns null if this
	 * cannot be achieved.
	 * 
	 * @param regularExpression The regular expression to comply to
	 * @return A regex compliant random string, or null
	 */
	private static String randomRegex(String regularExpression) {
		Generex generex = new Generex(regularExpression);
		// Generate random String matching the regex
		try {
			String result = generex.random();
			// strip boundaries
			if (result.startsWith("^") && result.endsWith("$")) {
				return StringUtils.substringBetween(result, "^", "$");
			}
			return result;
		} catch (@SuppressWarnings("unused") Exception e) {
			Util.LOGGER.warning("Couldnt generate compliant string for expression " + regularExpression);
		}
		return null;
	}

	private static String randomString(int length) {
		char[] guts = new char[length];
		for (int i = 0; i < length; i++) {
			guts[i] = Character.toChars(65 + (int) (RANDOM.nextDouble() * 26))[0];
		}

		return String.valueOf(guts);
	}

	/**
	 * Constructs a random string for the specified {@link Text} attribute. It will attempt
	 * to fill the text based on:
	 * <ul>
	 * <li>the presence of a file with the attribute name, e.g. firstName.txt
	 * <li>the presence of a format mask
	 * <li>a regular expression or other validator
	 * <li>random text
	 * 
	 * @param customerName The name of the current logged in user's customer to locate any test data files
	 * @param module The module this attribute belongs to
	 * @param document The document this attribute belongs to
	 * @param text The attribute to create the random data for
	 * @return A string containing random data for the text attribute
	 * @throws IOException
	 */
	public static String randomText(String customerName, Module module, Document document, Attribute attribute)
			throws IOException {
		if (attribute != null) {
			String fileName = null;
			Integer length = null;

			// check if there is a data map for this field
			if (module != null && document != null) {
				final String key = attributeKey(module, document, attribute.getName());
				if (DATA_MAP_CACHE.containsKey(key)) {
					fileName = DATA_MAP_CACHE.get(key);
					Util.LOGGER.fine(String.format("Loaded %s filename from cache", key));
				} else {
					String className = String.format("modules.%1$s.%2$s.%2$sFactory", module.getName(), document.getName());
					Util.LOGGER.fine("Looking for factory class " + className);
					try {
						Class<?> c = Thread.currentThread().getContextClassLoader().loadClass(className);
						if (c != null) {
							Util.LOGGER.fine("Found class " + c.getName());
							if (c.isAnnotationPresent(DataMap.class)) {
								DataMap annotation = c.getAnnotation(DataMap.class);
								Util.LOGGER.fine(
										String.format("attributeName: %s fileName: %s", annotation.attributeName(),
												annotation.fileName()));
								if (attribute.getName().equals(annotation.attributeName())) {
									fileName = annotation.fileName();
									DATA_MAP_CACHE.put(key, fileName);
								}
							} else if (c.isAnnotationPresent(SkyveFactory.class)) {
								SkyveFactory annotation = c.getAnnotation(SkyveFactory.class);
								DataMap[] values = annotation.value();
								for (DataMap map : values) {
									Util.LOGGER.fine(
											String.format("attributeName: %s fileName: %s", map.attributeName(), map.fileName()));
									if (attribute.getName().equals(map.attributeName())) {
										fileName = map.fileName();
										DATA_MAP_CACHE.put(key, fileName);
										break;
									}
								}
							}
						}
					} catch (@SuppressWarnings("unused") Exception e) {
						// couldn't find the extension file on the classpath
					}	
				}

				// check if there is a data file for this field
				Util.LOGGER.fine(String.format(
						"Looking for test data file in data/%s.txt", fileName != null ? fileName : attribute.getName()));
				String value = randomValueFromFile(customerName, module, document, attribute.getName(), fileName);
				if (value != null) {
					Util.LOGGER.fine(String.format("Random %s: %s", attribute.getName(), value));
					return value;
				}
			}

			// check if this string has a format mask
			if(attribute instanceof Text) {
				Text text = (Text) attribute;
				length = Integer.valueOf(text.getLength());
			
				if (text.getFormat() != null) {
					// check if it has a format mask and a regex, if so, prefer the regex
					if (text.getValidator() != null && text.getValidator().getRegularExpression() != null
							&& text.getValidator().getType() == null) {
						// return text matching the regex
						String xeger = randomRegex(text.getValidator().getRegularExpression());
						if (xeger != null) {
							return xeger;
						}
					}

					// return text matching the format mask
					return randomFormat(text.getFormat(), length.intValue());
				} else if (text.getValidator() != null && text.getValidator().getRegularExpression() != null
						&& text.getValidator().getType() == null) {
					// check if this string has a regex and no validator type
					String xeger = randomRegex(text.getValidator().getRegularExpression());
					if (xeger != null) {
						return xeger;
					}
				} else {
					// check if this is an email address
					if (text.getValidator() != null && ValidatorType.email.equals(text.getValidator().getType())) {
						return randomEmail(((LengthField) text).getLength());
					} else if (text.getValidator() != null && text.getValidator().getRegularExpression() != null) {
						// check if this string has a regex via a validator type
						String xeger = randomRegex(text.getValidator().getRegularExpression());
						if (xeger != null) {
							return xeger;
						}
					}
				}
			}
			
			// return random lorem ipsum text
			if (length == null) {
				// set an arbitrary max length for memo fields
				length = Integer.valueOf(2048);
			}
			String value = randomValueFromFile(null, null, null, LOREM);
			if (value != null) {
				String[] sentences = value.split("\\.");
				shuffleArray(sentences);
				int i = 0,
						min = length.intValue() / 3,
						r = RANDOM.nextInt(length.intValue() + 1 - min) + min;

				// keep adding sentences until we hit the length
				StringBuilder b = new StringBuilder();
				while (b.length() < r) {
					b.append(sentences[i]).append(".");
					i++;
					if (b.length() > r) {
						String out = b.toString();
						out = out.substring(0, out.length() < r ? out.length() : r).trim();
						if (out.indexOf(".") > 0) {
							// trim to last sentence boundary
							out = out.substring(0, out.lastIndexOf(".") + 1).trim();
						}
						if (out.length() > 0) {
							Util.LOGGER.fine(String.format("Random %s for %s with length %d(%d): %s",
									attribute.getAttributeType(),
									attribute.getName(),
									Integer.valueOf(r),
									length,
									out));
							return out;
						}
					}
				}
			}

			// return random text
			return randomString(length.intValue());
		}

		return null;
	}

	private static String randomText(Module module, Document document, Attribute attribute) throws IOException {
		String customerName = null;
		User user = CORE.getUser();
		if (user != null) {
			Customer customer = CORE.getCustomer();
			if (customer != null) {
				customerName = customer.getName();
			}
		}
		return randomText(customerName, module, document, attribute);
	}

	/**
	 * <p>
	 * Returns a random value from the test data file for the specified attribute if
	 * a data file exists, null otherwise. The file name is expected to be within a
	 * <code>data</code> directory on the classpath with the same name (case sensitive)
	 * as the attribute name.
	 * </p>
	 * 
	 * <p>
	 * E.g. <code>src/test/resources/data/postCode.txt</code>
	 * </p>
	 * 
	 * <p>
	 * The file will be cached the first time it is requested, and loaded from memory
	 * for subsequent random value requests.
	 * </p>
	 * 
	 * @param module The module this attribute belongs to
	 * @param document The document this attribute belongs to
	 * @param attributeName The attribute name to return the random value for
	 * @param fileName <em>Optional</em> The filename to load random values for, if it doesn't match the attribute name
	 * @return A random value from the data file if it exists, null otherwise
	 * @throws IOException
	 */
	private static String randomValueFromFile(String customerName, final Module module, final Document document,
			final String attributeName,
			final String... fileName) throws IOException {
		if (attributeName != null) {
			final String key = attributeKey(module, document, attributeName);

			List<String> values = null;
			if (DATA_CACHE.containsKey(key)) {
				values = DATA_CACHE.get(key);
				Util.LOGGER.fine(String.format("Loaded %s list from cache", key));
			} else {
				String fileToLoad = attributeName;
				if (fileName != null && fileName.length == 1 && fileName[0] != null) {
					fileToLoad = fileName[0];
				}

				// default the extension if none specified
				if (fileToLoad != null && !hasExtension(fileToLoad)) {
					fileToLoad = fileToLoad + ".txt";
				}

				Util.LOGGER.fine("Attempting to find on the classpath: " + String.format("data/%s", fileToLoad));
				File file = CORE.getRepository().findResourceFile(String.format("data/%s",
						fileToLoad),
						customerName,
						(module == null) ? null : module.getName());
				if ((file != null) && file.exists()) {
					try (InputStream inputStream = new FileInputStream(file)) {
						values = readFromInputStream(inputStream);
						DATA_CACHE.put(key, values);
						Util.LOGGER.fine(String.format("Caching attribute %s with filename %s", key, fileToLoad));
						if (values != null && values.size() > 0) {
							Util.LOGGER.fine(String.format("Loaded %s list from %s. Found %d values.", attributeName, fileToLoad,
									Integer.valueOf(values.size())));
						}
					}
				}
			}

			if (values != null) {
				// return random value or all of lorem
				if (attributeName.equals(LOREM)) {
					return values.stream().collect(Collectors.joining("\n"));
				}
				return values.get(RANDOM.nextInt(values.size()));
			}
		}

		return null;
	}

	/**
	 * Attempts to read a test data file from an input stream and stores each line
	 * as a string in a list.
	 * 
	 * @param inputStream The input stream to read from
	 * @return A list of strings for each line in the file, null if the input stream cannot be read
	 * @throws IOException
	 */
	private static List<String> readFromInputStream(InputStream inputStream) throws IOException {
		if (inputStream == null) {
			return null;
		}
		List<String> list = new ArrayList<>();
		try (BufferedReader br = new BufferedReader(new InputStreamReader(inputStream))) {
			String line;
			while ((line = br.readLine()) != null) {
				list.add(line);
			}
		}
		return list;
	}

	/**
	 * Shuffles an array using the <a href="http://en.wikipedia.org/wiki/Fisher-Yates_shuffle">Fisher–Yates shuffle</a>.
	 * 
	 * @param arr The array to shuffle
	 */
	private static void shuffleArray(String[] arr) {
		for (int i = arr.length - 1; i > 0; i--) {
			int index = RANDOM.nextInt(i + 1);
			// Simple swap
			String a = arr[index];
			arr[index] = arr[i];
			arr[i] = a;
		}
	}
	
	/**
	 * <p>
	 * Returns a random bean tuple from the specified document query
	 * </p>
	 * 
	 * <p>
	 * E.g. <code>src/test/resources/data/postCode.txt</code>
	 * </p>
	 * 
	 * @param q - the query to find a random value from
	 * @return - the random bean from the query result
	 */
    public static <T extends Bean> T findRandomDocumentQueryResult(DocumentQuery q) {
        AbstractDocumentQuery aq = ((AbstractDocumentQuery) q);

        aq.clearProjections();
        q.addAggregateProjection(AggregateFunction.Count, Bean.DOCUMENT_ID, "CountOfId");
        long count = q.scalarResult(Number.class).longValue();

        // we just need a random number
        if (count > Integer.MAX_VALUE) {
            count = Integer.MAX_VALUE;
        } else if (count == 0) {
            return null;
        }

        int randomIndex = new Random().nextInt((int) count - 1);

        // get the random record
        aq.clearProjections();
        q.setFirstResult(randomIndex);
        q.setMaxResults(1);

        return q.beanResult();
    }	
}
