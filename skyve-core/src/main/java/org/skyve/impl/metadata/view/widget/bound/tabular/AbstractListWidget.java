package org.skyve.impl.metadata.view.widget.bound.tabular;

import java.util.ArrayList;
import java.util.List;

import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.view.RelativeSize;
import org.skyve.impl.metadata.view.widget.FilterParameterImpl;
import org.skyve.impl.metadata.view.widget.bound.ParameterImpl;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.view.Filterable;
import org.skyve.metadata.view.Invisible;
import org.skyve.metadata.view.widget.FilterParameter;
import org.skyve.metadata.view.widget.bound.Parameter;
import org.skyve.util.Util;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Abstract JAXB base for query-driven list widgets ({@link ListGrid},
 * {@link ListRepeater}, {@link TreeGrid}) that render rows from a named
 * query or model.
 *
 * <p>Provides the query/model binding, filter parameters, relative sizing,
 * and visibility conditions shared by all list-type widgets.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see ListGrid
 * @see ListRepeater
 */
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE,
			propOrder = {"title", 
							"queryName",
							"modelName",
							"postRefreshConditionName",
							"filterParameters",
							"parameters",
							"pixelWidth",
							"responsiveWidth",
							"sm",
							"md",
							"lg",
							"xl",
							"percentageWidth",
							"minPixelWidth", 
							"maxPixelWidth", 
							"pixelHeight",
							"percentageHeight",
							"minPixelHeight", 
							"maxPixelHeight", 
							"invisibleConditionName",
							"visibleConditionName"})
public abstract class AbstractListWidget implements RelativeSize, Filterable, Invisible {
	private static final long serialVersionUID = 9068940194810436542L;

	private String title;
	
	private String queryName;
	private String modelName;
	private String postRefreshConditionName;
	
	private List<FilterParameter> filterParameters = new ArrayList<>();
	private List<Parameter> parameters = new ArrayList<>();
	
	private Integer pixelWidth;
	private Integer responsiveWidth;
	private Integer sm;
	private Integer md;
	private Integer lg;
	private Integer xl;
	private Integer percentageWidth;
	private Integer minPixelWidth;
	private Integer maxPixelWidth;

	private Integer pixelHeight;
	private Integer percentageHeight;
	private Integer minPixelHeight;
	private Integer maxPixelHeight;
	
	private String invisibleConditionName;

	/**
	 * Returns the configured list title.
	 *
	 * @return the title expression, or {@code null} when not set
	 */
	public String getTitle() {
		return title;
	}
	
	/**
	 * Returns the title localised for the current user locale.
	 *
	 * @return the localised title text
	 */
	public String getLocalisedTitle() {
		return Util.i18n(title);
	}

	/**
	 * Sets the list title after trimming and empty-string normalisation.
	 *
	 * @param title the title expression
	 */
	@XmlAttribute(required = false)
	public void setTitle(String title) {
		this.title = UtilImpl.processStringValue(title);
	}

	/**
	 * Returns the configured query name.
	 *
	 * @return the query name, or {@code null} when model-backed rendering is used
	 */
	public String getQueryName() {
		return queryName;
	}

	/**
	 * Sets the query name after trimming and empty-string normalisation.
	 *
	 * @param queryName the query name
	 */
	@XmlAttribute(name = "query")
	public void setQueryName(String queryName) {
		this.queryName = UtilImpl.processStringValue(queryName);
	}

	/**
	 * Returns the configured model name.
	 *
	 * @return the model name, or {@code null} when query-backed rendering is used
	 */
	public String getModelName() {
		return modelName;
	}

	/**
	 * Sets the model name after trimming and empty-string normalisation.
	 *
	 * @param modelName the model name
	 */
	@XmlAttribute(name = "model")
	public void setModelName(String modelName) {
		this.modelName = UtilImpl.processStringValue(modelName);
	}

	/**
	 * Returns the post-refresh condition expression.
	 *
	 * @return the post-refresh condition expression, or {@code null} when not set
	 */
	public String getPostRefreshConditionName() {
		return postRefreshConditionName;
	}

	/**
	 * Sets the post-refresh condition expression evaluated after data reload.
	 *
	 * @param refresh the post-refresh condition expression
	 */
	@XmlAttribute(name = "postRefresh")
	public void setPostRefreshConditionName(String refresh) {
		this.postRefreshConditionName = refresh;
	}

	/**
	 * Returns the mutable filter-parameter list used when executing the backing query or model.
	 *
	 * <p>Callers may add or remove entries before runtime rendering. The returned list is never {@code null}.
	 */
	@Override
	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE, 
					name = "filterParameter",
					type = FilterParameterImpl.class,
					required = false)
	public List<FilterParameter> getFilterParameters() {
		return filterParameters;
	}

	/**
	 * Returns the mutable runtime parameter list passed to the backing query or model.
	 *
	 * <p>The returned list is live and never {@code null}.
	 *
	 * @return mutable runtime parameters for the backing query or model
	 */
	@Override
	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE, 
					name = "parameter",
					type = ParameterImpl.class,
					required = false)
	public List<Parameter> getParameters() {
		return parameters;
	}

	@Override
	/**
	 * Returns the absolute pixel width, or {@code null} when not specified.
	 *
	 * @return the absolute pixel width
	 */
	public Integer getPixelWidth() {
		return pixelWidth;
	}

	/**
	 * Sets the absolute pixel width.
	 *
	 * @param pixelWidth the width in pixels
	 */
	@Override
	@XmlAttribute(required = false)
	public void setPixelWidth(Integer pixelWidth) {
		this.pixelWidth = pixelWidth;
	}

	/**
	 * Returns the responsive width.
	 *
	 * @return the responsive width units
	 */
	@Override
	public Integer getResponsiveWidth() {
		return responsiveWidth;
	}

	/**
	 * Sets the responsive width.
	 *
	 * @param responsiveWidth the responsive width units
	 */
	@Override
	@XmlAttribute(required = false)
	public void setResponsiveWidth(Integer responsiveWidth) {
		this.responsiveWidth = responsiveWidth;
	}

	/**
	 * Returns the small-breakpoint width override.
	 *
	 * @return the small-breakpoint width units
	 */
	@Override
	public Integer getSm() {
		return sm;
	}
	
	/**
	 * Sets the small-breakpoint width override.
	 *
	 * @param sm the small-breakpoint width units
	 */
	@Override
	@XmlAttribute(required = false)
	public void setSm(Integer sm) {
		this.sm = sm;
	}

	/**
	 * Returns the medium-breakpoint width override.
	 *
	 * @return the medium-breakpoint width units
	 */
	@Override
	public Integer getMd() {
		return md;
	}
	
	/**
	 * Sets the medium-breakpoint width override.
	 *
	 * @param md the medium-breakpoint width units
	 */
	@Override
	@XmlAttribute(required = false)
	public void setMd(Integer md) {
		this.md = md;
	}
	
	/**
	 * Returns the large-breakpoint width override.
	 *
	 * @return the large-breakpoint width units
	 */
	@Override
	public Integer getLg() {
		return lg;
	}
	
	/**
	 * Sets the large-breakpoint width override.
	 *
	 * @param lg the large-breakpoint width units
	 */
	@Override
	@XmlAttribute(required = false)
	public void setLg(Integer lg) {
		this.lg = lg;
	}

	/**
	 * Returns the extra-large-breakpoint width override.
	 *
	 * @return the extra-large-breakpoint width units
	 */
	@Override
	public Integer getXl() {
		return xl;
	}
	
	/**
	 * Sets the extra-large-breakpoint width override.
	 *
	 * @param xl the extra-large-breakpoint width units
	 */
	@Override
	@XmlAttribute(required = false)
	public void setXl(Integer xl) {
		this.xl = xl;
	}

	/**
	 * Returns the percentage width.
	 *
	 * @return the width percentage
	 */
	@Override
	public Integer getPercentageWidth() {
		return percentageWidth;
	}

	/**
	 * Sets the percentage width.
	 *
	 * @param percentageWidth the width percentage
	 */
	@Override
	@XmlAttribute(required = false)
	public void setPercentageWidth(Integer percentageWidth) {
		this.percentageWidth = percentageWidth;
	}

	/**
	 * Returns the minimum pixel width constraint.
	 *
	 * @return the minimum width in pixels, or {@code null} when unconstrained
	 */
	@Override
	public Integer getMinPixelWidth() {
		return minPixelWidth;
	}

	/**
	 * Sets the minimum pixel width constraint.
	 *
	 * @param minPixelWidth the minimum width in pixels
	 */
	@Override
	@XmlAttribute(required = false)
	public void setMinPixelWidth(Integer minPixelWidth) {
		this.minPixelWidth = minPixelWidth;
	}

	/**
	 * Returns the maximum pixel width constraint.
	 *
	 * @return the maximum width in pixels, or {@code null} when unconstrained
	 */
	@Override
	public Integer getMaxPixelWidth() {
		return maxPixelWidth;
	}

	/**
	 * Sets the maximum pixel width constraint.
	 *
	 * @param maxPixelWidth the maximum width in pixels
	 */
	@Override
	@XmlAttribute(required = false)
	public void setMaxPixelWidth(Integer maxPixelWidth) {
		this.maxPixelWidth = maxPixelWidth;
	}

	/**
	 * Returns the absolute pixel height, or {@code null} when not specified.
	 *
	 * @return the absolute pixel height
	 */
	@Override
	public Integer getPixelHeight() {
		return pixelHeight;
	}

	/**
	 * Sets the absolute pixel height.
	 *
	 * @param pixelHeight the height in pixels
	 */
	@Override
	@XmlAttribute(required = false)
	public void setPixelHeight(Integer pixelHeight) {
		this.pixelHeight = pixelHeight;
	}

	/**
	 * Returns the percentage height.
	 *
	 * @return the height percentage
	 */
	@Override
	public Integer getPercentageHeight() {
		return percentageHeight;
	}

	/**
	 * Sets the percentage height.
	 *
	 * @param percentageHeight the height percentage
	 */
	@Override
	@XmlAttribute(required = false)
	public void setPercentageHeight(Integer percentageHeight) {
		this.percentageHeight = percentageHeight;
	}

	/**
	 * Returns the minimum pixel height constraint.
	 *
	 * @return the minimum height in pixels, or {@code null} when unconstrained
	 */
	@Override
	public Integer getMinPixelHeight() {
		return minPixelHeight;
	}

	/**
	 * Sets the minimum pixel height constraint.
	 *
	 * @param minPixelHeight the minimum height in pixels
	 */
	@Override
	@XmlAttribute(required = false)
	public void setMinPixelHeight(Integer minPixelHeight) {
		this.minPixelHeight = minPixelHeight;
	}

	/**
	 * Returns the maximum pixel height constraint.
	 *
	 * @return the maximum height in pixels, or {@code null} when unconstrained
	 */
	@Override
	public Integer getMaxPixelHeight() {
		return maxPixelHeight;
	}

	/**
	 * Sets the maximum pixel height constraint.
	 *
	 * @param maxPixelHeight the maximum height in pixels
	 */
	@Override
	@XmlAttribute(required = false)
	public void setMaxPixelHeight(Integer maxPixelHeight) {
		this.maxPixelHeight = maxPixelHeight;
	}

	/**
	 * Returns the internal invisible condition expression.
	 *
	 * @return the invisible condition expression
	 */
	@Override
	public String getInvisibleConditionName() {
		return invisibleConditionName;
	}

	/**
	 * Sets the invisible condition expression directly.
	 *
	 * @param invisibleConditionName the invisible condition expression
	 */
	@Override
	@XmlAttribute(name = "invisible", required = false)
	public void setInvisibleConditionName(String invisibleConditionName) {
		this.invisibleConditionName = UtilImpl.processStringValue(invisibleConditionName);
	}

	/**
	 * JAXB-only placeholder for {@code visible}; value is derived via {@link #setVisibleConditionName(String)}.
	 *
	 * @return always {@code null}; JAXB writes via {@code setVisibleConditionName}
	 */
	// to enable JAXB XML marshaling
	@SuppressWarnings("static-method")
	String getVisibleConditionName() {
		return null;
	}

	/**
	 * Sets the visible condition by storing its negation as the internal invisible condition.
	 *
	 * @param visibleConditionName a condition expression that enables visibility
	 */
	@Override
	@XmlAttribute(name = "visible", required = false)
	public void setVisibleConditionName(String visibleConditionName) {
		this.invisibleConditionName = BindUtil.negateCondition(UtilImpl.processStringValue(visibleConditionName));
	}
}
