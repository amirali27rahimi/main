#include <iostream>
#include <string>
#include <map>
#include <vector>
#include <algorithm>
#include <stdexcept>
#include <cctype>
#include <cmath>
#include <sstream>
#include <set>

using namespace std;

// Custom stack implementation
template<typename T>
class Stack {
private:
    vector<T> elements;
public:
    void push(const T& item) {
        elements.push_back(item);
    }
    void pop() {
        if (elements.empty()) throw runtime_error("Stack underflow");
        elements.pop_back();
    }
    T& top() {
        if (elements.empty()) throw runtime_error("Stack is empty");
        return elements.back();
    }
    bool empty() const {
        return elements.empty();
    }
};

// Custom queue implementation
template<typename T>
class Queue {
private:
    vector<T> elements;
public:
    void push(const T& item) {
        elements.push_back(item);
    }
    void pop() {
        if (elements.empty()) throw runtime_error("Queue underflow");
        elements.erase(elements.begin());
    }
    T& front() {
        if (elements.empty()) throw runtime_error("Queue is empty");
        return elements.front();
    }
    bool empty() const {
        return elements.empty();
    }
    int size() const { 
        return elements.size(); 
    }
};

int factorial(int n) {
    if (n < 0) throw invalid_argument("Negative input for factorial");
    if (n == 0) return 1;
    return n * factorial(n - 1);
}

double eval_custom(string expr, map<string, double>& values);

string replace_all_vars(string expr, map<string, double>& values) {
    for (const auto& v : values) {
        string var = v.first;
        string var_upper = var;
        transform(var_upper.begin(), var_upper.end(), var_upper.begin(), ::toupper);
        size_t pos = 0;
        while ((pos = expr.find(var_upper, pos)) != string::npos) {
            if ((pos == 0 || !isalnum(expr[pos - 1])) && (pos + var_upper.size() == expr.size() || !isalnum(expr[pos + var_upper.size()]))) {
                expr.replace(pos, var_upper.length(), to_string(v.second));
                pos += to_string(v.second).length();
            } else {
                pos += var_upper.length();
            }
        }
    }
    return expr;
}

double apply_operation(double a, double b, char op) {
    switch(op) {
        case '+': return a + b;
        case '-': return a - b;
        case '*': return a * b;
        case '/': if (b != 0) return a / b;
                  else throw invalid_argument("Division by zero");
        case '^': return pow(a, b);
        default: throw invalid_argument("Unsupported operation");
    }
}

double eval_custom(string expr, map<string, double>& values) {
    expr = replace_all_vars(expr, values);

    // Handle factorial
    size_t pos;
    while ((pos = expr.find('!')) != string::npos) {
        int i = pos - 1;
        while (i >= 0 && (isdigit(expr[i]) || isalpha(expr[i]))) {
            i--;
        }
        string number = expr.substr(i + 1, pos - i - 1);
        double fact_value = 0;
        if (isdigit(number[0])) {
            fact_value = factorial(stoi(number));
        } else {
            string uppercase_var = number;
            transform(uppercase_var.begin(), uppercase_var.end(), uppercase_var.begin(), ::toupper);
            if (values.find(uppercase_var) != values.end()) {
                fact_value = factorial(static_cast<int>(values[uppercase_var]));
            } else {
                throw invalid_argument("Undefined variable: " + uppercase_var);
            }
        }
        expr.replace(i + 1, pos - i, to_string(fact_value));
    }

    expr = replace_all_vars(expr, values);

    // Evaluate expression using custom stack
    Stack<double> values_stack;
    Stack<char> ops_stack;
    stringstream ss(expr);
    char c;

    while (ss >> c) {
        if (isdigit(c) || c == '.') {
            ss.putback(c);
            double value;
            ss >> value;
            values_stack.push(value);
        } else if (isalpha(c)) {
            string var_name;
            ss.putback(c);
            ss >> var_name;
            transform(var_name.begin(), var_name.end(), var_name.begin(), ::toupper);
            if (values.find(var_name) != values.end()) {
                values_stack.push(values[var_name]);
            } else {
                throw invalid_argument("Undefined variable: " + var_name);
            }
        } else if (c == '(') {
            ops_stack.push(c);
        } else if (c == ')') {
            while (!ops_stack.empty() && ops_stack.top() != '(') {
                double b = values_stack.top(); values_stack.pop();
                double a = values_stack.top(); values_stack.pop();
                char op = ops_stack.top(); ops_stack.pop();
                values_stack.push(apply_operation(a, b, op));
            }
            ops_stack.pop();
        } else {
            while (!ops_stack.empty() && ops_stack.top() != '(') {
                double b = values_stack.top(); values_stack.pop();
                double a = values_stack.top(); values_stack.pop();
                char op = ops_stack.top(); ops_stack.pop();
                values_stack.push(apply_operation(a, b, op));
            }
            ops_stack.push(c);
        }
    }

    while (!ops_stack.empty()) {
        double b = values_stack.top(); values_stack.pop();
        double a = values_stack.top(); values_stack.pop();
        char op = ops_stack.top(); ops_stack.pop();
        values_stack.push(apply_operation(a, b, op));
    }

    return values_stack.top();
}

void compute_and_print_values(vector<string> expressions) {
    map<string, double> values = { {"PI", 3.14159}, {"EN", 2.71828} };
    set<string> defined_variables;
    defined_variables.insert("PI");
    defined_variables.insert("EN");

    Queue<string> pending_expressions;
    map<string, set<string>> dependencies;

    for (const auto& expr : expressions) {
        pending_expressions.push(expr);
    }

    while (!pending_expressions.empty()) {
        int size = pending_expressions.size();
        bool any_updated = false;

        for (int i = 0; i < size; ++i) {
            string expression = pending_expressions.front();
            pending_expressions.pop();

            try {
                size_t pos = expression.find('=');
                if (pos == string::npos) throw invalid_argument("Invalid expression");

                string var = expression.substr(0, pos);
                for (auto& c : var) c = toupper(c);  // Convert to uppercase
                string expr = expression.substr(pos + 1);

                // Extract dependencies
                set<string> deps;
                stringstream ss(expr);
                string token;
                while (ss >> token) {
                    if (isalpha(token[0])) {
                        transform(token.begin(), token.end(), token.begin(), ::toupper);
                        deps.insert(token);
                    }
                }
                dependencies[var] = deps;

                // Check if all dependencies are resolved
                bool can_evaluate = true;
                for (const auto& dep : deps) {
                    if (defined_variables.find(dep) == defined_variables.end()) {
                        can_evaluate = false;
                        break;
                    }
                }

                if (!can_evaluate) {
                    pending_expressions.push(expression);
                    continue;
                }

                values[var] = eval_custom(expr, values);
                defined_variables.insert(var);
                any_updated = true;
            } catch (...) {
                pending_expressions.push(expression);
            }
        }

        if (!any_updated) {
            throw runtime_error("Circular dependency detected or undefined variables.");
        }
    }

    vector<pair<string, double>> sorted_values;
    for (const auto& v : values) {
        if (v.first != "PI" && v.first != "EN") {
            sorted_values.push_back(v);
        }
    }

    sort(sorted_values.begin(), sorted_values.end());

    for (const auto& v : sorted_values) {
        cout << v.first << " = " << fixed << v.second << endl;
    }
}

int main() {
    int num_lines;
    cin >> num_lines;
    cin.ignore();

    vector<string> expressions;
    for (int i = 0; i < num_lines; ++i) {
        string expression;
        getline(cin, expression);
        expressions.push_back(expression);
    }

    compute_and_print_values(expressions);

    return 0;
}
