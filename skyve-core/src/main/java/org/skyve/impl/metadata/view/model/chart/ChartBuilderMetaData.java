package org.skyve.impl.metadata.view.model.chart;

import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.view.model.ModelMetaData;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.view.model.chart.Bucket;
import org.skyve.persistence.DocumentQuery.AggregateFunction;
import org.skyve.util.JSON;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlElementRef;
import jakarta.xml.bind.annotation.XmlElementRefs;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;

/**
 * JAXB root descriptor for a chart model definition used by the
 * {@link org.skyve.impl.metadata.view.widget.Chart} widget.
 *
 * <p>Declares the data model name, chart type, bucket strategy, optional
 * ordering/top selection, and other model parameters used to generate chart
 * series at runtime.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 */
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE,
			propOrder = {"title",
							"label",
							"moduleName",
							"documentName",
							"queryName",
							"categoryBinding",
							"categoryBucket",
							"valueBinding",
							"valueFunction",
							"top",
							"order",
							"JFreeChartPostProcessorClassName",
							"primeFacesChartPostProcessorClassName"})
public class ChartBuilderMetaData implements ModelMetaData {
	private static final long serialVersionUID = -6525994383289095450L;

	private String modelName;
	private String title;
	private String label;
	private String moduleName;
	private String documentName;
	private String queryName;
	private String categoryBinding;
	private Bucket categoryBucket;
	private String valueBinding;
	private AggregateFunction valueFunction;
	private ChartBuilderTopMetaData top;
	private ChartBuilderOrderMetaData order;
	private String jFreeChartPostProcessorClassName;
	private String primeFacesChartPostProcessorClassName;
	
	/**
	 * Returns the name this inline chart model is known by, to the view that declares it and to clients.
	 *
	 * <p>The name is the identifier of the model's definition (see {@link BindUtil#identifier(String)}),
	 * so it depends only on the definition: the same definition has the same name wherever
	 * and whenever it is asked for, and a different definition has a different one.
	 *
	 * <p>Side effects: the name is worked out on first use and kept. Mutating the model
	 * after that does not change it.
	 *
	 * @return the 27 character model name
	 * @throws MetaDataException if the definition cannot be marshalled to derive the name from
	 */
	@Override
	@XmlTransient
	public String getModelName() {
		if (modelName == null) {
			String definition;
			try {
				definition = JSON.marshall(this);
			}
			catch (Exception e) {
				throw new MetaDataException("ChartBuilderMetaData cannot generate a model name", e);
			}
			modelName = BindUtil.identifier(definition);
		}
		return modelName;
	}

	public String getTitle() {
		return title;
	}
	@XmlAttribute(required = false)
	public void setTitle(String title) {
		this.title = UtilImpl.processStringValue(title);
	}

	public String getLabel() {
		return label;
	}
	@XmlAttribute(required = true)
	public void setLabel(String label) {
		this.label = UtilImpl.processStringValue(label);
	}
	
	public String getModuleName() {
		return moduleName;
	}
	@XmlAttribute(required = true)
	public void setModuleName(String moduleName) {
		this.moduleName = UtilImpl.processStringValue(moduleName);
	}
	
	public String getDocumentName() {
		return documentName;
	}
	@XmlAttribute
	public void setDocumentName(String documentName) {
		this.documentName = UtilImpl.processStringValue(documentName);
	}

	public String getQueryName() {
		return queryName;
	}
	@XmlAttribute
	public void setQueryName(String queryName) {
		this.queryName = UtilImpl.processStringValue(queryName);
	}

	public String getCategoryBinding() {
		return categoryBinding;
	}
	@XmlAttribute(required = true)
	public void setCategoryBinding(String categoryBinding) {
		this.categoryBinding = UtilImpl.processStringValue(categoryBinding);
	}

	public Bucket getCategoryBucket() {
		return categoryBucket;
	}
	
	@XmlElementRefs({@XmlElementRef(type = NoBucketMetaData.class),
						@XmlElementRef(type = NumericMultipleBucketMetaData.class),
						@XmlElementRef(type = NumericRangeBucketMetaData.class),
						@XmlElementRef(type = TemporalBucketMetaData.class),
						@XmlElementRef(type = TextLengthBucketMetaData.class),
						@XmlElementRef(type = TextStartsWithBucketMetaData.class)})
	public void setCategoryBucket(Bucket categoryBucket) {
		this.categoryBucket = categoryBucket;
		if (categoryBucket instanceof NumericRangeBucketMetaData range) {
			range.convert();
		}
	}

	public String getValueBinding() {
		return valueBinding;
	}
	@XmlAttribute(required = true)
	public void setValueBinding(String valueBinding) {
		this.valueBinding = UtilImpl.processStringValue(valueBinding);
	}
	
	public AggregateFunction getValueFunction() {
		return valueFunction;
	}
	@XmlAttribute
	public void setValueFunction(AggregateFunction valueFunction) {
		this.valueFunction = valueFunction;
	}

	public ChartBuilderTopMetaData getTop() {
		return top;
	}
	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE)
	public void setTop(ChartBuilderTopMetaData top) {
		this.top = top;
	}
	
	public ChartBuilderOrderMetaData getOrder() {
		return order;
	}
	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE)
	public void setOrder(ChartBuilderOrderMetaData order) {
		this.order = order;
	}

	public String getJFreeChartPostProcessorClassName() {
		return jFreeChartPostProcessorClassName;
	}
	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE)
	public void setJFreeChartPostProcessorClassName(String jFreeChartPostProcessorClassName) {
		this.jFreeChartPostProcessorClassName = UtilImpl.processStringValue(jFreeChartPostProcessorClassName);
	}

	public String getPrimeFacesChartPostProcessorClassName() {
		return primeFacesChartPostProcessorClassName;
	}
	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE)
	public void setPrimeFacesChartPostProcessorClassName(String primeFacesChartPostProcessorClassName) {
		this.primeFacesChartPostProcessorClassName = UtilImpl.processStringValue(primeFacesChartPostProcessorClassName);
	}
}
