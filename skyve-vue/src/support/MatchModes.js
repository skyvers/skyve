
const FilterMatchMode = {

    STARTS_WITH: 'iStartsWith',
    CONTAINS: 'iContains',
    NOT_CONTAINS: 'iNotContains',
    ENDS_WITH: 'iEndsWith',
    TEXT_EQUALS: 'iEquals',
    TEXT_NOT_EQUALS: 'iNotEqual',

    EQUALS: 'equals',
    NOT_EQUALS: 'notEqual',
    LESS_THAN: 'lessThan',
    LESS_THAN_OR_EQUAL_TO: 'lessOrEqual',
    GREATER_THAN: 'greaterThan',
    GREATER_THAN_OR_EQUAL_TO: 'greaterOrEqual',
};

const _DATE_MODES = [
    { label: 'Equals', value: FilterMatchMode.EQUALS },
    { label: 'Not Equals', value: FilterMatchMode.NOT_EQUALS },
    { label: 'Before', value: FilterMatchMode.LESS_THAN },
    { label: 'After', value: FilterMatchMode.GREATER_THAN },
];

export const MatchModes = {

    MODES: FilterMatchMode,
    text: [
        { label: 'Starts With', value: FilterMatchMode.STARTS_WITH },
        { label: 'Contains', value: FilterMatchMode.CONTAINS },
        { label: 'Not Contains', value: FilterMatchMode.NOT_CONTAINS },
        { label: 'Ends With', value: FilterMatchMode.ENDS_WITH },
        { label: 'Equals', value: FilterMatchMode.TEXT_EQUALS },
        { label: 'Not Equals', value: FilterMatchMode.TEXT_NOT_EQUALS },
    ],
    numeric: [
        { label: 'Less Than', value: FilterMatchMode.LESS_THAN },
        { label: 'Less Than Or Equal', value: FilterMatchMode.LESS_THAN_OR_EQUAL_TO },
        { label: 'Greater Than', value: FilterMatchMode.GREATER_THAN },
        { label: 'Greater Than Or Equal', value: FilterMatchMode.GREATER_THAN_OR_EQUAL_TO },
        { label: 'Equals', value: FilterMatchMode.EQUALS },
        { label: 'Not Equals', value: FilterMatchMode.NOT_EQUALS },
    ],
    date: _DATE_MODES,
    dateTime: _DATE_MODES,
    timestamp: _DATE_MODES,
    time: [
        { label: 'Equals', value: FilterMatchMode.EQUALS },
    ],
    enum: [
        { label: 'Equals', value: FilterMatchMode.EQUALS },
        { label: 'Not Equals', value: FilterMatchMode.NOT_EQUALS },
    ],
    boolean: [
        { label: 'Equals', value: FilterMatchMode.EQUALS },
        { label: 'Not Equals', value: FilterMatchMode.NOT_EQUALS },
    ]
}