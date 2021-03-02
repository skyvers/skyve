package services.report.freemarker;

import java.io.IOException;
import java.io.Writer;
import java.util.Date;
import java.util.Iterator;
import java.util.Map;

import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.io.WKTWriter;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.messages.SkyveException;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.TimeOnly;
import org.skyve.domain.types.Timestamp;
import org.skyve.metadata.customer.Customer;

import freemarker.core.Environment;
import freemarker.template.TemplateDirectiveBody;
import freemarker.template.TemplateDirectiveModel;
import freemarker.template.TemplateException;
import freemarker.template.TemplateModel;
import freemarker.template.TemplateModelException;
import freemarker.template.utility.DeepUnwrap;

/**
 * FreeMarker user-defined directive that performs a Skyve display format on the specified value
 * based on the Java class.
 *
 * <p>
 * <b>Directive info</b>
 * </p>
 *
 * <p>
 * Directive parameters:
 * <ul>
 * <li><code>value</code>: The value to format
 * </ul>
 * <p>
 * Loop variables: None
 * <p>
 * Directive nested content: No
 * 
 * <p>
 * Usage: <code><@sqlformat value=bean.dateCreated /></code>.
 * </p>
 */
public class SqlFormatDirective implements TemplateDirectiveModel {

	private static final String PARAM_NAME_VALUE = "value";

	@SuppressWarnings("rawtypes")
	@Override
	public void execute(Environment env, Map params, TemplateModel[] loopVars, TemplateDirectiveBody body)
			throws TemplateException, IOException {
		if (params.isEmpty()) {
			throw new TemplateModelException("This directive requires parameters.");
		}

		if (loopVars.length != 0) {
			throw new TemplateModelException("This directive doesn't allow loop variables.");
		}

		// If there is non-empty nested content:
		if (body != null) {
			throw new TemplateModelException("This directive doesn't allow nested content.");
		}

		// process the parameters
		Object valueParam = null;

		Iterator paramIter = params.entrySet().iterator();
		while (paramIter.hasNext()) {
			Map.Entry ent = (Map.Entry) paramIter.next();

			String paramName = (String) ent.getKey();
			TemplateModel paramValue = (TemplateModel) ent.getValue();

			if (paramName.equals(PARAM_NAME_VALUE)) {
				// unwrap to try get the skyve object
				valueParam = DeepUnwrap.permissiveUnwrap(paramValue);
			} else {
				throw new TemplateModelException("Unsupported parameter: " + paramName);
			}
		}

		// do the actual directive execution
		Writer out = env.getOut();
		if (valueParam != null) {
			out.write(toDisplay(valueParam));
		}
	}

	private String toDisplay(final Object value) {
		String result = "";
		try {
			Customer customer = CORE.getCustomer();
			if (customer == null) {
				throw new TemplateModelException("No customer available, unable to format.");
			}

			if (value == null) {
				// do nothing as result is already empty
			} else if (value instanceof java.sql.Timestamp) {
				java.sql.Timestamp timestamp = (java.sql.Timestamp) value;
				result = customer.getDefaultTimestampConverter().toDisplayValue(new Timestamp(timestamp));
			} else if (value instanceof java.sql.Date) {
				java.sql.Date date = (java.sql.Date) value;
				result = customer.getDefaultDateConverter().toDisplayValue(new DateOnly(date));
			} else if (value instanceof DateOnly) {
				result = customer.getDefaultDateConverter().toDisplayValue((DateOnly) value);
			} else if (value instanceof TimeOnly) {
				result = customer.getDefaultTimeConverter().toDisplayValue((TimeOnly) value);
			} else if (value instanceof DateTime) {
				result = customer.getDefaultDateTimeConverter().toDisplayValue((DateTime) value);
			} else if (value instanceof Timestamp) {
				result = customer.getDefaultTimestampConverter().toDisplayValue((Timestamp) value);
			} else if (value instanceof Date) {
				result = customer.getDefaultDateTimeConverter().toDisplayValue(new DateTime((Date) value));
			} else if (value instanceof Boolean) {
				result = (((Boolean) value).booleanValue() ? "Yes" : "No");
			} else if (value instanceof Geometry) {
				result = new WKTWriter().write((Geometry) value);
			} else {
				result = value.toString();
			}
		} catch (Exception e) {
			if (e instanceof SkyveException) {
				throw (SkyveException) e;
			}

			throw new DomainException(e);
		}

		return result;
	}

}
