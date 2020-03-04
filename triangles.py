import math
import random

# 45,45,90
# H = Leg * sqrt(2)
# 30,60,90
# LL = SL * sqrt(3)
# H = 2 * SL

print('first:')
typ = random.choice(['45', '30'])
if typ == '45':
    want = random.choice(['leg', 'H'])
    x = random.choice(range(1, 11))
    print(f'type: 45, leg: {x}, H: {x} * sqrt(2), want: {want}')
else:
    typ2 = random.choice(['LL = SL * sqrt(3)', 'H = 2 * SL'])
    if typ2 == 'LL = SL * sqrt(3)':
        want = random.choice(['LL', 'SL'])
        x = random.choice(range(1, 11))
        print(f'type (1): {typ}, type (2): {typ2}, SL: {x}, LL: {x} * sqrt(3), want: {want}')
    else:
        want = random.choice(['H', 'SL'])
        x = random.choice(range(1, 11))
        print(f'type (2): {typ}, type (2): {typ2}, SL: {x}, H: 2 * {x}, want: {want}')

print()

print('second:')
typ = random.choice(['sin', 'cos', 'tan'])
typ = 'tan'
if typ == 'sin':
    angle = random.choice(range(10, 80))
    want = random.choice(['opp', 'hyp'])
    if want == 'opp':
        hyp = random.choice(range(5, 11))
        opp = hyp * math.sin(math.radians(angle))
    else: # want == 'hyp'
        opp = random.choice(range(5, 11))
        hyp = opp / math.sin(math.radians(angle))
    check = (math.sin(math.radians(angle)), 1.0 * opp / hyp)
    print(f'type: {typ}, angle: {angle}, opp: {opp}, hyp: {hyp}, want: {want}, check: {check}')
elif typ == 'cos':
    angle = random.choice(range(10, 80))
    want = random.choice(['adj', 'hyp'])
    if want == 'adj':
        hyp = random.choice(range(5, 11))
        adj = hyp * math.cos(math.radians(angle))
    else: # want == 'hyp'
        adj = random.choice(range(5, 11))
        hyp = adj / math.cos(math.radians(angle))
    check = (math.cos(math.radians(angle)), 1.0 * adj / hyp)
    print(f'type: {typ}, angle: {angle}, adj: {adj}, hyp: {hyp}, want: {want}, check: {check}')
else:
    angle = random.choice(range(10, 80))
    want = random.choice(['opp', 'adj'])
    if want == 'opp':
        adj = random.choice(range(5, 11))
        opp = adj * math.tan(math.radians(angle))
    else: # want == 'adj'
        opp = random.choice(range(5, 11))
        adj = opp / math.tan(math.radians(angle))
    check = (math.tan(math.radians(angle)), 1.0 * opp / adj)
    print(f'type: {typ}, angle: {angle}, opp: {opp}, adj: {adj}, want: {want}, check: {check}')
