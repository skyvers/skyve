ViewModel interface extends MetaData and adds postConstruct(Customer customer, boolean runtime).
ChartModel, ComparisonModel, ListModel and MapModel implement ViewModel.

Domain Generation instantiates the ViewModels and calls postConstruct method with the runtime argument of false.
Access Control does the same.
For the above 2 use cases, the models do not have their dependencies injected via CDI and the runtime argument can be used to conditionally control the use of Skyve services from CORE and EXT, like Persistence etc.

Any Skyve function that uses a ViewModel also calls postConstruct method with the runtime argument of true.

If ListModel.getDrivingDocument() yields null then Domain Generation can't validate column bindings and parameter bindings and access control list items cannot be added.
The null is allowed to cater for late-resolved driving documents in dynamic list models in tandem with the <s:view/> dynamic attribute. 
