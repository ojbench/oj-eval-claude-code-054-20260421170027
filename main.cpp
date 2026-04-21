#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include <map>
#include <sstream>

using namespace std;

struct Term {
    long long a;
    int b, c, d;

    Term(long long _a = 0, int _b = 0, int _c = 0, int _d = 0) : a(_a), b(_b), c(_c), d(_d) {}

    bool is_like(const Term& other) const {
        return b == other.b && c == other.c && d == other.d;
    }

    bool operator<(const Term& other) const {
        if (b != other.b) return b > other.b;
        if (c != other.c) return c > other.c;
        return d > other.d;
    }
};

struct Poly {
    vector<Term> terms;

    Poly() {}
    Poly(const vector<Term>& t) : terms(t) {}

    void simplify() {
        if (terms.empty()) return;
        sort(terms.begin(), terms.end());
        vector<Term> result;
        for (const auto& term : terms) {
            if (term.a == 0) continue;
            if (!result.empty() && result.back().is_like(term)) {
                result.back().a += term.a;
            } else {
                result.push_back(term);
            }
        }
        terms.clear();
        for (const auto& term : result) {
            if (term.a != 0) terms.push_back(term);
        }
    }

    Poly operator+(const Poly& other) const {
        Poly res = *this;
        res.terms.insert(res.terms.end(), other.terms.begin(), other.terms.end());
        res.simplify();
        return res;
    }

    Poly operator-(const Poly& other) const {
        Poly res = *this;
        for (auto t : other.terms) {
            res.terms.push_back(Term(-t.a, t.b, t.c, t.d));
        }
        res.simplify();
        return res;
    }

    Poly operator*(const Poly& other) const {
        Poly res;
        for (const auto& t1 : terms) {
            for (const auto& t2 : other.terms) {
                res.terms.push_back(Term(t1.a * t2.a, t1.b + t2.b, t1.c + t2.c, t1.d + t2.d));
            }
        }
        res.simplify();
        return res;
    }

    Poly derivate() const {
        Poly res;
        for (const auto& t : terms) {
            if (t.b > 0) {
                res.terms.push_back(Term(t.a * t.b, t.b - 1, t.c, t.d));
            }
            if (t.c > 0) {
                res.terms.push_back(Term(t.a * t.c, t.b, t.c - 1, t.d + 1));
            }
            if (t.d > 0) {
                res.terms.push_back(Term(-t.a * t.d, t.b, t.c + 1, t.d - 1));
            }
        }
        res.simplify();
        return res;
    }

    string to_string() const {
        if (terms.empty()) return "0";
        string res = "";
        bool first = true;
        for (const auto& t : terms) {
            long long a = t.a;
            if (a > 0 && !first) res += "+";
            if (a == -1) {
                if (t.b == 0 && t.c == 0 && t.d == 0) res += "-1";
                else res += "-";
            } else if (a == 1) {
                if (t.b == 0 && t.c == 0 && t.d == 0) res += "1";
            } else {
                res += std::to_string(a);
            }

            if (t.b > 0) {
                res += "x";
                if (t.b > 1) res += "^" + std::to_string(t.b);
            }
            if (t.c > 0) {
                res += "sin";
                if (t.c > 1) res += "^" + std::to_string(t.c);
                res += "x";
            }
            if (t.d > 0) {
                res += "cos";
                if (t.d > 1) res += "^" + std::to_string(t.d);
                res += "x";
            }
            first = false;
        }
        return res;
    }
};

struct Frac {
    Poly p, q;

    Frac() {
        q.terms.push_back(Term(1, 0, 0, 0));
    }

    Frac(long long val) {
        p.terms.push_back(Term(val, 0, 0, 0));
        q.terms.push_back(Term(1, 0, 0, 0));
    }

    Frac(Poly _p, Poly _q) : p(_p), q(_q) {}

    Frac operator+(const Frac& other) const {
        return Frac(p * other.q + other.p * q, q * other.q);
    }

    Frac operator-(const Frac& other) const {
        return Frac(p * other.q - other.p * q, q * other.q);
    }

    Frac operator*(const Frac& other) const {
        return Frac(p * other.p, q * other.q);
    }

    Frac operator/(const Frac& other) const {
        return Frac(p * other.q, q * other.p);
    }

    Frac derivate() const {
        return Frac(p.derivate() * q - q.derivate() * p, q * q);
    }

    void output() const {
        string sp = p.to_string();
        string sq = q.to_string();

        bool p_needs_paren = (p.terms.size() > 1);
        bool q_needs_paren = (q.terms.size() > 1);

        if (sq == "1") {
            cout << sp << endl;
        } else if (sp == "0") {
            cout << "0" << endl;
        } else {
            if (p_needs_paren) cout << "(" << sp << ")";
            else cout << sp;
            cout << "/";
            if (q_needs_paren) cout << "(" << sq << ")";
            else cout << sq;
            cout << endl;
        }
    }
};

int pos = 0;
string input_str;

Frac parse_expression();
Frac parse_term();
Frac parse_factor();
Poly parse_poly_term();

Frac parse_expression() {
    Frac res = parse_term();
    while (pos < input_str.length() && (input_str[pos] == '+' || input_str[pos] == '-')) {
        char op = input_str[pos++];
        Frac next = parse_term();
        if (op == '+') res = res + next;
        else res = res - next;
    }
    return res;
}

Frac parse_term() {
    Frac res = parse_factor();
    while (pos < input_str.length() && (input_str[pos] == '*' || input_str[pos] == '/')) {
        char op = input_str[pos++];
        Frac next = parse_factor();
        if (op == '*') res = res * next;
        else res = res / next;
    }
    return res;
}

long long get_num() {
    bool neg = false;
    if (pos < input_str.length() && input_str[pos] == '-') {
        neg = true;
        pos++;
    }
    if (pos >= input_str.length() || !isdigit(input_str[pos])) {
        return neg ? -1 : 1;
    }
    long long res = 0;
    while (pos < input_str.length() && isdigit(input_str[pos])) {
        res = res * 10 + (input_str[pos++] - '0');
    }
    return neg ? -res : res;
}

Poly parse_poly_term() {
    long long a = 1;
    int b = 0, c = 0, d = 0;

    if (pos < input_str.length() && (isdigit(input_str[pos]) || (input_str[pos] == '-' && pos + 1 < input_str.length() && isdigit(input_str[pos+1])))) {
        a = get_num();
    } else if (pos < input_str.length() && input_str[pos] == '-') {
        a = -1;
        pos++;
    }

    while (pos < input_str.length()) {
        if (input_str[pos] == 'x') {
            pos++;
            int exp = 1;
            if (pos < input_str.length() && input_str[pos] == '^') {
                pos++;
                exp = 0;
                while (pos < input_str.length() && isdigit(input_str[pos])) {
                    exp = exp * 10 + (input_str[pos++] - '0');
                }
            }
            b += exp;
        } else if (pos + 2 < input_str.length() && input_str.substr(pos, 3) == "sin") {
            pos += 3;
            int exp = 1;
            if (pos < input_str.length() && input_str[pos] == '^') {
                pos++;
                exp = 0;
                while (pos < input_str.length() && isdigit(input_str[pos])) {
                    exp = exp * 10 + (input_str[pos++] - '0');
                }
            }
            if (pos < input_str.length() && input_str[pos] == 'x') pos++;
            c += exp;
        } else if (pos + 2 < input_str.length() && input_str.substr(pos, 3) == "cos") {
            pos += 3;
            int exp = 1;
            if (pos < input_str.length() && input_str[pos] == '^') {
                pos++;
                exp = 0;
                while (pos < input_str.length() && isdigit(input_str[pos])) {
                    exp = exp * 10 + (input_str[pos++] - '0');
                }
            }
            if (pos < input_str.length() && input_str[pos] == 'x') pos++;
            d += exp;
        } else {
            break;
        }
    }
    Poly res;
    res.terms.push_back(Term(a, b, c, d));
    return res;
}

Frac parse_factor() {
    if (input_str[pos] == '(') {
        pos++;
        Frac res = parse_expression();
        pos++; // ')'
        return res;
    } else {
        Poly p = parse_poly_term();
        return Frac(p, Poly({Term(1, 0, 0, 0)}));
    }
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(NULL);
    if (!(cin >> input_str)) return 0;
    pos = 0;
    Frac f = parse_expression();
    f.output();
    f.derivate().output();
    return 0;
}
