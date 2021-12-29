package org.skyve.impl.bind;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.Date;
import java.util.Map;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import javax.el.ELManager;
import javax.el.ELProcessor;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.Decimal;
import org.skyve.domain.types.Decimal10;
import org.skyve.domain.types.Decimal2;
import org.skyve.domain.types.Decimal5;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.util.ExpressionEvaluator;

public class ELExpressionEvaluator extends ExpressionEvaluator {
	public static final String EL_PREFIX = "el";
	public static final String RTEL_PREFIX = "rtel";
	
	private boolean typesafe = false;
	
	public ELExpressionEvaluator(boolean typesafe) {
		this.typesafe = typesafe;
	}

	@Override
	public Object evaluateWithoutPrefix(String expression, Bean bean) {
		ELProcessor elp = newSkyveEvaluationProcessor(bean);
		return elp.eval(expression);
	}

	@Override
	public String formatWithoutPrefix(String expression, Bean bean) {
		return BindUtil.toDisplay(CORE.getCustomer(), null, null, evaluateWithoutPrefix(expression, bean));
	}
	
	@Override
	public String validateWithoutPrefix(String expression,
											Class<?> returnType,
											Customer customer,
											Module module,
											Document document) {
		String result = null;

		if (typesafe) {
			try {
				// type-safe (el) starts with the document, if no document, no bean defined in the context
				ELProcessor elp = newSkyveValidationProcessor(customer, document);
				Object evaluation = elp.eval(expression);
				if (returnType != null) {
					Class<?> type = null;
					if (evaluation instanceof DocumentImpl) {
						type = ((DocumentImpl) evaluation).getBeanClass(customer);
					}
					else if (evaluation instanceof Class<?>) {
						type = (Class<?>) evaluation;
					}
					else if (evaluation != null) {
						type = evaluation.getClass();
					}
					if ((type != null) && (! returnType.isAssignableFrom(type))) {
						result = expression + " returns an instance of type " + type +
								" that is incompatible with required return type of " + returnType;
					}
				}
			}
			catch (Exception e) {
				e.printStackTrace();
				result = e.getMessage();
				if (result == null) {
					result = expression + " is malformed and caused an exception " + e.getClass();
				}
			}
		}
		
		return result;
	}
	
	public static ELProcessor newSkyveValidationProcessor(@Nonnull Customer customer, @Nullable Document document) {
		ELProcessor result = setupProcessor(customer, document, UserImpl.class, Map.class);
		result.getELManager().addELResolver(new ValidationELResolver(customer));
		return result;
	}
	
	public static ELProcessor newSkyveEvaluationProcessor(@Nullable Bean bean) {
		ELProcessor result = setupProcessor(null, bean, CORE.getUser(), CORE.getStash());
		result.getELManager().addELResolver(new BindingELResolver());
		return result;
	}
	
	private static ELProcessor setupProcessor(@SuppressWarnings("unused") Customer customer, Object bean, Object user, Object stash) {
		ELProcessor result = new ELProcessor();
		if (bean != null) {
			result.defineBean("bean", bean);
		}
		result.defineBean("user", user);
		result.defineBean("stash", stash);
		
		try {
			Class<?> functions = ELFunctions.class;
			result.defineFunction("", "", functions.getMethod("newDateOnly"));
			result.defineFunction("", "", functions.getMethod("newDateOnlyFromMillis", Long.TYPE));
			result.defineFunction("", "", functions.getMethod("newDateOnlyFromDate", Date.class));
			result.defineFunction("", "", functions.getMethod("newDateOnlyFromSerializedForm", String.class));
			result.defineFunction("", "", functions.getMethod("newDateOnlyFromLocalDate", LocalDate.class));
			result.defineFunction("", "", functions.getMethod("newDateOnlyFromLocalDateTime", LocalDateTime.class));

			result.defineFunction("", "", functions.getMethod("newDateTime"));
			result.defineFunction("", "", functions.getMethod("newDateTimeFromMillis", Long.TYPE));
			result.defineFunction("", "", functions.getMethod("newDateTimeFromDate", Date.class));
			result.defineFunction("", "", functions.getMethod("newDateTimeFromSerializedForm", String.class));
			result.defineFunction("", "", functions.getMethod("newDateTimeFromLocalDate", LocalDate.class));
			result.defineFunction("", "", functions.getMethod("newDateTimeFromLocalDateTime", LocalDateTime.class));

			result.defineFunction("", "", functions.getMethod("newTimeOnly"));
			result.defineFunction("", "", functions.getMethod("newTimeOnlyFromMillis", Long.TYPE));
			result.defineFunction("", "", functions.getMethod("newTimeOnlyFromDate", Date.class));
			result.defineFunction("", "", functions.getMethod("newTimeOnlyFromComponents", Integer.TYPE, Integer.TYPE, Integer.TYPE));
			result.defineFunction("", "", functions.getMethod("newTimeOnlyFromSerializedForm", String.class));
			result.defineFunction("", "", functions.getMethod("newTimeOnlyFromLocalTime", LocalTime.class));
			result.defineFunction("", "", functions.getMethod("newTimeOnlyFromLocalDateTime", LocalDateTime.class));
			
			result.defineFunction("", "", functions.getMethod("newTimestamp"));
			result.defineFunction("", "", functions.getMethod("newTimestampFromMillis", Long.TYPE));
			result.defineFunction("", "", functions.getMethod("newTimestampFromDate", Date.class));
			result.defineFunction("", "", functions.getMethod("newTimestampFromSerializedForm", String.class));
			result.defineFunction("", "", functions.getMethod("newTimestampFromLocalDate", LocalDate.class));
			result.defineFunction("", "", functions.getMethod("newTimestampFromLocalDateTime", LocalDateTime.class));

			result.defineFunction("", "", functions.getMethod("newDecimal2", Double.TYPE));
			result.defineFunction("", "", functions.getMethod("newDecimal2FromBigDecimal", BigDecimal.class));
			result.defineFunction("", "", functions.getMethod("newDecimal2FromDecimal", Decimal.class));
			result.defineFunction("", "", functions.getMethod("newDecimal2FromString", String.class));
			
			result.defineFunction("", "", functions.getMethod("newDecimal5", Double.TYPE));
			result.defineFunction("", "", functions.getMethod("newDecimal5FromBigDecimal", BigDecimal.class));
			result.defineFunction("", "", functions.getMethod("newDecimal5FromDecimal", Decimal.class));
			result.defineFunction("", "", functions.getMethod("newDecimal5FromString", String.class));

			result.defineFunction("", "", functions.getMethod("newDecimal10", Double.TYPE));
			result.defineFunction("", "", functions.getMethod("newDecimal10FromBigDecimal", BigDecimal.class));
			result.defineFunction("", "", functions.getMethod("newDecimal10FromDecimal", Decimal.class));
			result.defineFunction("", "", functions.getMethod("newDecimal10FromString", String.class));

			result.defineFunction("", "", functions.getMethod("newOptimisticLock", String.class, Date.class));
			result.defineFunction("", "", functions.getMethod("newOptimisticLockFromString", String.class));
			result.defineFunction("", "", functions.getMethod("newGeometry", String.class));
		}
		catch (NoSuchMethodException | SecurityException e) {
			throw new DomainException("Cannot define EL functions", e);
		}
		
		final ELManager elManager = result.getELManager();
		elManager.importClass(Decimal2.class.getCanonicalName());
		elManager.importClass(Decimal5.class.getCanonicalName());
		elManager.importClass(Decimal10.class.getCanonicalName());

/* TODO resolve this inner enum class problem.
	Cannot import the domain classes here coz its not on the classpath for the maven mojo.
	And I cant get it to work at runtime for a nested enum class either using $ or . in the class name.
	We might have to use the normal defaulting for default values and allow built-in EL String coercion for enums in expressions.

		Class<?> classToImport = null;
		
		if (bean instanceof Document) { // could be a Document in validation mode
			DocumentImpl d = (DocumentImpl) bean;
			if (! d.isDynamic()) {
				try {
					classToImport = d.getBeanClass(customer);
				} catch (ClassNotFoundException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
		}
		else if ((bean != null) && (! (bean instanceof MapBean))) { // not dynamic
			classToImport = bean.getClass();
		}

		if (classToImport != null) {
			String name = classToImport.getCanonicalName();
			elManager.importClass(name);
			for (Class<?> innerClass : classToImport.getDeclaredClasses()) {
				String innerName = innerClass.getCanonicalName();// name + '$' + innerClass.getSimpleName();
				elManager.importClass(innerName);
			}
			classToImport.getSuperclass();
		}
*/		
		return result;
	}
}
