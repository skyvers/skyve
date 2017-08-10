package org.skyve.impl.util;

import java.security.SecureRandom;
import java.util.Date;
import java.util.List;
import java.util.UUID;

import org.apache.commons.lang3.StringUtils;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.model.document.AssociationImpl;
import org.skyve.impl.metadata.model.document.field.LengthField;
import org.skyve.impl.metadata.model.document.field.Text;
import org.skyve.impl.metadata.model.document.field.TextFormat;
import org.skyve.impl.metadata.model.document.field.validator.TextValidator.ValidatorType;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Collection;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.util.Binder;
import org.skyve.util.Util;

import com.mifmif.common.regex.Generex;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;

public class TestUtil {

	private static final SecureRandom random = new SecureRandom();

	private static final String NUMBERS = "0123456789";
	private static final String LETTERS = "abcdefghijklmnopqrstuvwxyz";
	private static final String ALPHA_NUMERIC = LETTERS + NUMBERS;

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
	 * Update an attribute on the given bean with a random value
	 * 
	 * @param bean The bean containing the to update
	 * @param attribute The current value of the attribute of the bean to modify
	 * @return The bean with a modified attribute with a different random value if possible
	 */
	@SuppressWarnings({ "unchecked", "boxing" })
	public static <T extends PersistentBean> T updateAttribute(T bean, Attribute attribute) {
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
			case dateTime:
			case time:
			case timestamp:
				Date futureDate = new Date();
				TimeUtil.addHours(futureDate, random.nextInt(10 + 1));
				BindUtil.convertAndSet(bean, name, futureDate);
				break;
			case decimal10:
			case decimal2:
			case decimal5:
			case integer:
			case longInteger:
				BindUtil.convertAndSet(bean, name, new Integer((int) Math.random() * 10000));
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
						new Coordinate(random.nextInt(10), random.nextInt(10))));
				break;
			case id:
				BindUtil.set(bean, name, UUID.randomUUID().toString());
				break;
			case markup:
			case memo:
				BindUtil.set(bean, name, randomString(((int) (Math.random() * 255)) + 1));
				break;
			case text:
				BindUtil.set(bean, name, randomString(((LengthField) attribute).getLength()));
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

	@SuppressWarnings({ "incomplete-switch", "boxing" }) // content type missing from switch statement
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
				case integer:
				case longInteger:
					BindUtil.convertAndSet(result, name, new Integer(random.nextInt(10000)));
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
					BindUtil.set(result, name, randomString(((int) (Math.random() * 255)) + 1));
					break;
				case text:
					Text text = (Text) attribute;

					// check if this string has a format mask
					if (text.getFormat() != null) {
						// check if it has a format mask and a regex, if so, prefer the regex
						if (text.getValidator() != null && text.getValidator().getRegularExpression() != null
								&& text.getValidator().getType() == null) {
							// return text matching the regex
							String xeger = randomRegex(text.getValidator().getRegularExpression());
							if (xeger != null) {
								BindUtil.set(result, name, xeger);
								continue;
							}
						}

						// return text matching the format mask
						BindUtil.set(result, name, randomFormat(text.getFormat()));
					} else if (text.getValidator() != null && text.getValidator().getRegularExpression() != null
							&& text.getValidator().getType() == null) {
						// check if this string has a regex and no validator type
						String xeger = randomRegex(text.getValidator().getRegularExpression());
						if (xeger != null) {
							BindUtil.set(result, name, xeger);
							continue;
						}
					} else {
						// check if this is an email address
						if (text.getValidator() != null && ValidatorType.email.equals(text.getValidator().getType())) {
							BindUtil.set(result, name, randomEmail(((LengthField) attribute).getLength()));
						} else if (text.getValidator() != null && text.getValidator().getRegularExpression() != null) {
							// check if this string has a regex via a validator type
							String xeger = randomRegex(text.getValidator().getRegularExpression());
							if (xeger != null) {
								BindUtil.set(result, name, xeger);
								continue;
							}
						} else {
							// return random text
							BindUtil.set(result, name, randomString(((LengthField) attribute).getLength()));
						}
					}
			}
		}
		return result;
	}

	private static String randomEmail(int length) {
		int addressLength = (int) Math.floor((length - 2) / 2);
		int domainLength = (int) Math.floor((length - 2) / 2) - 2;

		char[] address = new char[addressLength];
		for (int i = 0; i < addressLength; i++) {
			address[i] = Character.toChars(65 + (int) (Math.random() * 26))[0];
		}

		char[] domain = new char[domainLength];
		for (int i = 0; i < domainLength; i++) {
			domain[i] = Character.toChars(65 + (int) (Math.random() * 26))[0];
		}

		char[] code = new char[2];
		for (int i = 0; i < 2; i++) {
			code[i] = Character.toChars(65 + (int) (Math.random() * 26))[0];
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
	private static <T extends Enum<?>> T randomEnum(Class<T> clazz, Integer currentValue) {
		int x;
		if (currentValue != null) {
			do {
				x = random.nextInt(clazz.getEnumConstants().length);
			} while (x == currentValue);
		} else {
			x = random.nextInt(clazz.getEnumConstants().length);
		}

		return clazz.getEnumConstants()[x];
	}

	/**
	 * Returns a random string which complies to the format mask
	 * of the text attribute
	 * 
	 * @param textFormat The format to comply to
	 * @return A format compliant random string
	 */
	private static String randomFormat(TextFormat textFormat) {

		String mask = textFormat.getMask();
		String out = new String();

		for (int i = 0; i < mask.length(); i++) {
			char c = mask.charAt(i);
			switch (c) {
				case '#':
					out += NUMBERS.charAt(random.nextInt(NUMBERS.length()));
					break;
				case 'A':
					out += ALPHA_NUMERIC.charAt(random.nextInt(ALPHA_NUMERIC.length()));
					break;
				case 'L':
					out += LETTERS.charAt(random.nextInt(LETTERS.length()));
					break;
				default:
					out += c;
					break;
			}
		}

		return out;
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
		} catch (Exception e) {
			Util.LOGGER.warning("Couldnt generate compliant string for expression " + regularExpression);
		}
		return null;
	}

	private static String randomString(int length) {
		char[] guts = new char[length];
		for (int i = 0; i < length; i++) {
			guts[i] = Character.toChars(65 + (int) (Math.random() * 26))[0];
		}

		return String.valueOf(guts);
	}

}
